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
{-# LANGUAGE OverloadedStrings     #-}

module Game.Werewolf.Engine (
    -- * Loop
    checkStage, checkGameOver,

    -- * Game
    startGame,
) where

import Control.Lens         hiding (cons, isn't)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List.Extra
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Game.Werewolf.Game     hiding (doesPlayerExist, getAllowedVoters, getPendingVoters,
                                         getVoteResult, hasAngelWon, hasAnyoneWon, hasVillagersWon,
                                         hasWerewolvesWon, killPlayer)
import           Game.Werewolf.Messages
import           Game.Werewolf.Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role     hiding (name)
import qualified Game.Werewolf.Role     as Role
import           Game.Werewolf.Util

import Prelude hiding (round)

checkStage :: (MonadState Game m, MonadWriter [Message] m) => m ()
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

checkStage' :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkStage' = use stage >>= \stage' -> case stage' of
    DefendersTurn -> do
        whenM (has (players . defenders . dead) <$> get) advanceStage

        whenM (isJust <$> use protect) advanceStage

    DevotedServantsTurn -> do
        whenM (has (players . devotedServants . dead) <$> get) advanceStage

        whenM (has devotedServants <$> getVoteResult)   advanceStage
        whenM (has devotedServants <$> getPassers)      advanceStage

    GameOver -> return ()

    Lynching -> do
        getVoteResult >>= lynchVotees

        allVoters       <- ifM (use villageIdiotRevealed)
            (uses players $ filter (isn't villageIdiot))
            (use players)
        allowedVoters   .= allVoters ^.. traverse . alive . name

        votes .= Map.empty

        advanceStage

    ScapegoatsTurn -> unlessM (use scapegoatBlamed) $ do
        allowedVoters' <- use allowedVoters
        tell [scapegoatChoseAllowedVotersMessage allowedVoters']

        advanceStage

    SeersTurn -> do
        whenM (has (players . seers . dead) <$> get) advanceStage

        whenJustM (use see) $ \targetName -> do
            seer    <- findPlayerBy_ role seerRole
            target  <- findPlayerBy_ name targetName

            tell [playerSeenMessage (seer ^. name) target]

            advanceStage

    Sunrise -> do
        round += 1

        whenJustM (preuse $ players . angels . alive) $ \angel ->
            unless (is villager angel) $ do
                tell [angelJoinedVillagersMessage]

                setPlayerAllegiance (angel ^. name) Villagers

        advanceStage

    Sunset -> do
        whenJustM (use roleModel) $ \roleModelsName -> do
            wildChild <- findPlayerBy_ role wildChildRole

            whenM (isPlayerDead roleModelsName &&^ return (is alive wildChild) &&^ return (is villager wildChild)) $ do
                aliveWerewolfNames <- toListOf (players . werewolves . alive . name) <$> get

                setPlayerAllegiance (wildChild ^. name) Werewolves

                tell $ wildChildJoinedPackMessages (wildChild ^. name) aliveWerewolfNames

        advanceStage

    UrsussGrunt -> do
        bearTamer   <- findPlayerBy_ role bearTamerRole
        players'    <- getAdjacentAlivePlayers (bearTamer ^. name)

        when (has werewolves players') $ tell [ursusGruntsMessage]

        advanceStage

    VillagesTurn -> whenM (null <$> liftM2 intersect getAllowedVoters getPendingVoters) $ do
        tell . map (uncurry playerMadeLynchVoteMessage) =<< uses votes Map.toList

        advanceStage

    WerewolvesTurn -> whenM (none (is werewolf) <$> getPendingVoters) $ do
        getVoteResult >>= devourVotees

        protect .= Nothing
        votes .= Map.empty

        advanceStage

    WildChildsTurn -> do
        whenM (has (players . wildChildren . dead) <$> get) advanceStage

        whenM (isJust <$> use roleModel) advanceStage

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
        whenM (has witches <$> getPassers)      advanceStage

    WolfHoundsTurn -> do
        whenM (has (players . wolfHounds . dead) <$> get) advanceStage

        whenJustM (use allegianceChosen) $ \allegiance -> do
            wolfHound <- findPlayerBy_ role wolfHoundRole

            setPlayerAllegiance (wolfHound ^. name) allegiance

            advanceStage

lynchVotees :: (MonadState Game m, MonadWriter [Message] m) => [Player] -> m ()
lynchVotees [votee]
    | is villageIdiot votee = do
        villageIdiotRevealed .= True

        tell [villageIdiotLynchedMessage $ votee ^. name]
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
    passes  .= []
    see     .= Nothing

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
applyEvent NoDevourEvent            = tell [noPlayerDevouredMessage]
applyEvent (PoisonEvent targetName) = do
    target <- findPlayerBy_ name targetName

    killPlayer targetName
    tell [playerPoisonedMessage target]

checkGameOver :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkGameOver = whenM hasAnyoneWon $ stage .= GameOver >> get >>= tell . gameOverMessages

startGame :: (MonadError [Message] m, MonadWriter [Message] m) => Text -> [Player] -> m Game
startGame callerName players = do
    when (playerNames /= nub playerNames)   $ throwError [privateMessage callerName "Player names must be unique."]
    when (length players < 7)               $ throwError [privateMessage callerName "Must have at least 7 players."]
    forM_ restrictedRoles $ \role' ->
        when (length (players ^.. traverse . filteredBy role role') > 1) $
            throwError [privateMessage callerName $ T.concat ["Cannot have more than 1 ", role' ^. Role.name, "."]]

    let game = newGame players

    tell $ newGameMessages game

    return game
    where
        playerNames = players ^.. names
