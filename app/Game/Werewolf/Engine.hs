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

import Game.Werewolf.Game     hiding (getAllowedVoters, getPendingVoters, getVoteResult,
                               hasAnyoneWon, hasFallenAngelWon, hasVillagersWon, hasWerewolvesWon)
import Game.Werewolf.Messages
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role     hiding (name)
import Game.Werewolf.Util

import Prelude hiding (round)

checkStage :: (MonadRandom m, MonadState Game m, MonadWriter [Message] m) => m ()
checkStage = do
    game <- get
    checkBoots >> checkStage' >> checkEvents
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
    FerinasGrunt -> do
        druid       <- findPlayerBy_ role druidRole
        players'    <- filter (isn't alphaWolf) <$> getAdjacentAlivePlayers (druid ^. name)

        when (has werewolves players' || has lycans players') $ tell [ferinaGruntsMessage]

        advanceStage

    GameOver -> return ()

    HuntersTurn1 -> whenM (use hunterRetaliated) advanceStage

    HuntersTurn2 -> whenM (use hunterRetaliated) advanceStage

    Lynching -> do
        getVoteResult >>= lynchVotees

        allVoters       <- ifM (use jesterRevealed)
            (uses players $ filter (isn't jester))
            (use players)
        allowedVoters   .= allVoters ^.. traverse . alive . name

        votes .= Map.empty

        advanceStage

    OraclesTurn -> do
        whenM (has (players . oracles . dead) <$> get) advanceStage

        whenM (isJust <$> use divine) advanceStage

    OrphansTurn -> do
        whenM (has (players . orphans . dead) <$> get) advanceStage

        whenM (isJust <$> use roleModel) advanceStage

    ProtectorsTurn -> do
        whenM (has (players . protectors . dead) <$> get) advanceStage

        whenM (isJust <$> use protect) advanceStage

    ScapegoatsTurn -> unlessM (use scapegoatBlamed) $ do
        allowedVoters' <- use allowedVoters
        tell [scapegoatChoseAllowedVotersMessage allowedVoters']

        advanceStage

    SeersTurn -> do
        whenM (has (players . seers . dead) <$> get) advanceStage

        whenM (isJust <$> use see) advanceStage

    Sunrise -> do
        round += 1

        whenJustM (preuse $ players . seers . alive) $ \seer -> do
            target <- use see >>= findPlayerBy_ name . fromJust

            when (is alive target) $ tell [playerSeenMessage (seer ^. name) target]

        whenJustM (preuse $ players . oracles . alive) $ \oracle -> do
            target <- use divine >>= findPlayerBy_ name . fromJust

            when (is alive target) $ tell [playerDivinedMessage (oracle ^. name) target]

        see     .= Nothing
        divine  .= Nothing

        advanceStage

    Sunset -> do
        whenJustM (use roleModel) $ \roleModelsName -> do
            orphan <- findPlayerBy_ role orphanRole

            whenM (isPlayerDead roleModelsName &&^ return (is alive orphan) &&^ return (is villager orphan)) $ do
                aliveWerewolfNames <- toListOf (players . werewolves . alive . name) <$> get

                setPlayerAllegiance (orphan ^. name) Werewolves

                tell $ orphanJoinedPackMessages (orphan ^. name) aliveWerewolfNames

        advanceStage

    VillageDrunksTurn -> do
        aliveWerewolfNames <- toListOf (players . werewolves . alive . name) <$> get

        randomAllegiance <- getRandomAllegiance
        players . villageDrunks . role . allegiance .= randomAllegiance

        villageDrunk <- findPlayerBy_ role villageDrunkRole

        if is villager villageDrunk
            then tell [villageDrunkJoinedVillageMessage $ villageDrunk ^. name]
            else tell $ villageDrunkJoinedPackMessages (villageDrunk ^. name) aliveWerewolfNames

        advanceStage

    VillagesTurn -> whenM (null <$> liftM2 intersect getAllowedVoters getPendingVoters) $ do
        tell . map (uncurry $ playerMadeLynchVoteMessage Nothing) =<< uses votes Map.toList

        advanceStage

    WerewolvesTurn -> whenM (none (is werewolf) <$> getPendingVoters) $ do
        getVoteResult >>= devourVotees

        protect .= Nothing
        votes .= Map.empty

        advanceStage

    WitchsTurn -> do
        whenM (has (players . witches . dead) <$> get) advanceStage

        whenJustM (use poison) $ \targetName -> do
            events %= (++ [PoisonEvent targetName])
            poison .= Nothing

        whenM (use heal) $ do
            devourEvent <- fromJust <$> preuse (events . traverse . filtered (is _DevourEvent))

            events  %= cons NoDevourEvent . delete devourEvent
            heal    .= False

        whenM (use healUsed &&^ use poisonUsed) advanceStage
        whenM (use passed)                      advanceStage

lynchVotees :: (MonadState Game m, MonadWriter [Message] m) => [Player] -> m ()
lynchVotees [votee]
    | is jester votee       = do
        jesterRevealed .= True

        tell [jesterLynchedMessage $ votee ^. name]
    | is fallenAngel votee  = do
        fallenAngelLynched .= True

        tell [playerLynchedMessage votee]
    | otherwise             = do
        killPlayer (votee ^. name)
        tell [playerLynchedMessage votee]
lynchVotees _       = preuse (players . scapegoats . alive) >>= \mScapegoat -> case mScapegoat of
    Just scapegoat  -> do
        scapegoatBlamed .= True

        killPlayer (scapegoat ^. name)
        tell [scapegoatLynchedMessage (scapegoat ^. name)]
    _               -> tell [noPlayerLynchedMessage]

devourVotees :: (MonadState Game m, MonadWriter [Message] m) => [Player] -> m ()
devourVotees [votee]    = ifM (uses protect $ maybe False (== votee ^. name))
    (events %= cons NoDevourEvent)
    (events %= cons (DevourEvent $ votee ^. name))
devourVotees _          = events %= cons NoDevourEvent

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

checkEvents :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkEvents = do
    (available, pending) <- use events >>= partitionM eventAvailable

    events .= pending

    mapM_ applyEvent available

eventAvailable :: MonadState Game m => Event -> m Bool
eventAvailable (DevourEvent _)  = isSunrise
eventAvailable NoDevourEvent    = isSunrise
eventAvailable (PoisonEvent _)  = isSunrise

applyEvent :: (MonadState Game m, MonadWriter [Message] m) => Event -> m ()
applyEvent (DevourEvent targetName) = do
    target <- findPlayerBy_ name targetName

    killPlayer targetName
    tell [playerDevouredMessage target]

    when (is medusa target) . whenJustM (getFirstAdjacentAliveWerewolf targetName) $ \werewolf -> do
        killPlayer (werewolf ^. name)
        tell [playerTurnedToStoneMessage werewolf]
applyEvent NoDevourEvent            = tell [noPlayerDevouredMessage]
applyEvent (PoisonEvent targetName) = do
    target <- findPlayerBy_ name targetName

    killPlayer targetName
    tell [playerPoisonedMessage target]

checkGameOver :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkGameOver = whenM hasAnyoneWon $ stage .= GameOver >> get >>= tell . gameOverMessages
