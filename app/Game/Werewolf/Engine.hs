{-|
Module      : Game.Werewolf.Engine
Description : Engine functions.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Engine functions.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Engine (
    -- * Loop
    checkStage, checkGameOver,
) where

import Control.Lens         hiding (cons, isn't)
import Control.Lens.Extra
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer

import           Data.List.Extra
import qualified Data.Map        as Map
import           Data.Maybe

-- TODO (hjw): remove Message.Command
import Game.Werewolf.Game            hiding (getAllowedVoters, getPendingVoters, hasAnyoneWon,
                                      hasFallenAngelWon, hasVillagersWon, hasWerewolvesWon)
import Game.Werewolf.Message.Command
import Game.Werewolf.Message.Engine
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role            hiding (name)
import Game.Werewolf.Util

import Prelude hiding (round)

checkStage :: (MonadRandom m, MonadState Game m, MonadWriter [Message] m) => m ()
checkStage = do
    game <- get
    checkBoots >> checkStage'
    game' <- get

    when (game /= game') checkStage

checkBoots :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkBoots = do
    alivePlayerCount <- length . toListOf (players . traverse . alive) <$> get

    booteeNames <- uses boots $ Map.keys . Map.filter (\voters -> length voters > alivePlayerCount `div` 2)
    bootees     <- mapM (findPlayerBy_ name) booteeNames

    forM_ (filter (is alive) bootees) $ \bootee -> do
        tell [playerBootedMessage bootee]

        removePlayer (bootee ^. name)

checkStage' :: (MonadRandom m, MonadState Game m, MonadWriter [Message] m) => m ()
checkStage' = use stage >>= \stage' -> case stage' of
    DruidsTurn -> do
        druid       <- findPlayerBy_ role druidRole
        players'    <- filter (isn't alphaWolf) <$> getAdjacentAlivePlayers (druid ^. name)

        when (has werewolves players' || has lycans players') $ tell [ferinaGruntsMessage]

        advanceStage

    GameOver -> return ()

    HuntersTurn1 -> whenM (use hunterRetaliated) advanceStage

    HuntersTurn2 -> whenM (use hunterRetaliated) advanceStage

    Lynching -> do
        lynchVotee =<< preuse votee

        allVoters       <- ifM (use jesterRevealed)
            (uses players $ filter (isn't jester))
            (use players)
        allowedVoters   .= allVoters ^.. traverse . alive . name

        votes .= Map.empty

        advanceStage

    OraclesTurn -> do
        whenM (hasuse $ players . oracles . dead) advanceStage

        whenM (isJust <$> use divine) advanceStage

    OrphansTurn -> do
        whenM (hasuse $ players . orphans . dead) advanceStage

        whenM (isJust <$> use roleModel) advanceStage

    ProtectorsTurn -> do
        whenM (hasuse $ players . protectors . dead) advanceStage

        whenM (isJust <$> use protect) advanceStage

    ScapegoatsTurn -> unlessM (use scapegoatBlamed) $ do
        game <- get
        tell [scapegoatChoseAllowedVotersMessage game]

        advanceStage

    SeersTurn -> do
        whenM (hasuse $ players . seers . dead) advanceStage

        whenM (isJust <$> use see) advanceStage

    Sunrise -> do
        round += 1

        devourVotee =<< preuse votee

        whenJustM (use poison) $ \targetName -> do
            target <- findPlayerBy_ name targetName

            killPlayer targetName
            tell [playerPoisonedMessage target]

        whenJustM (preuse $ players . seers . alive) $ \seer -> do
            target <- use see >>= findPlayerBy_ name . fromJust

            when (is alive target) $ tell [playerSeenMessage (seer ^. name) target]

        whenJustM (preuse $ players . oracles . alive) $ \oracle -> do
            target <- use divine >>= findPlayerBy_ name . fromJust

            when (is alive target) $ tell [playerDivinedMessage (oracle ^. name) target]

        divine  .= Nothing
        poison  .= Nothing
        protect .= Nothing
        see     .= Nothing
        votes   .= Map.empty

        advanceStage

    Sunset -> do
        whenJustM (use roleModel) $ \roleModelsName -> do
            orphan <- findPlayerBy_ role orphanRole

            whenM (isPlayerDead roleModelsName &&^ return (is alive orphan) &&^ return (is villager orphan)) $ do
                setPlayerAllegiance (orphan ^. name) Werewolves

                tell . orphanJoinedPackMessages (orphan ^. name) =<< get

        advanceStage

    VillageDrunksTurn -> do
        randomAllegiance <- getRandomAllegiance
        players . villageDrunks . role . allegiance .= randomAllegiance

        villageDrunk <- findPlayerBy_ role villageDrunkRole

        if is villager villageDrunk
            then tell [villageDrunkJoinedVillageMessage $ villageDrunk ^. name]
            else tell . villageDrunkJoinedPackMessages (villageDrunk ^. name) =<< get

        advanceStage

    VillagesTurn -> whenM (null <$> liftM2 intersect getAllowedVoters getPendingVoters) $ do
        uses votes Map.toList >>= mapM_ (\(voterName, voteeName) -> do
            voter <- findPlayerBy_ name voterName
            votee <- findPlayerBy_ name voteeName

            tell [playerMadeLynchVoteMessage Nothing voter votee]
            )

        advanceStage

    WerewolvesTurn -> whenM (none (is werewolf) <$> getPendingVoters) $ do
        whenM (liftM2 (==) (use protect) (preuses votee $ view name)) $ votes .= Map.empty

        advanceStage

    WitchsTurn -> do
        whenM (hasuse $ players . witches . dead) advanceStage

        whenM (use healUsed &&^ use poisonUsed) advanceStage
        whenM (use passed)                      advanceStage

lynchVotee :: (MonadState Game m, MonadWriter [Message] m) => Maybe Player -> m ()
lynchVotee (Just votee)
    | is jester votee       = do
        jesterRevealed .= True

        tell . (:[]) . jesterLynchedMessage =<< get
    | is fallenAngel votee  = do
        fallenAngelLynched .= True

        tell [playerLynchedMessage votee]
    | otherwise             = do
        killPlayer (votee ^. name)
        tell [playerLynchedMessage votee]
lynchVotee _            = preuse (players . scapegoats . alive) >>= \mScapegoat -> case mScapegoat of
    Just scapegoat  -> do
        scapegoatBlamed .= True

        killPlayer (scapegoat ^. name)
        tell . (:[]) . scapegoatLynchedMessage =<< get
    _               -> tell [noPlayerLynchedMessage]

devourVotee :: (MonadState Game m, MonadWriter [Message] m) => Maybe Player -> m ()
devourVotee Nothing         = tell [noPlayerDevouredMessage]
devourVotee (Just votee)    = do
    killPlayer (votee ^. name)
    tell [playerDevouredMessage votee]

    when (is medusa votee) . whenJustM (getFirstAdjacentAliveWerewolf $ votee ^. name) $ \werewolf -> do
        killPlayer (werewolf ^. name)
        tell [playerTurnedToStoneMessage werewolf]

advanceStage :: (MonadState Game m, MonadWriter [Message] m) => m ()
advanceStage = do
    game        <- get
    nextStage   <- ifM hasAnyoneWon
        (return GameOver)
        (return . head $ filter (stageAvailable game) (drop1 $ dropWhile (game ^. stage /=) stageCycle))

    stage   .= nextStage
    boots   .= Map.empty
    passed  .= False

    tell . stageMessages =<< get

checkGameOver :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkGameOver = whenM hasAnyoneWon $ stage .= GameOver >> get >>= tell . gameOverMessages
