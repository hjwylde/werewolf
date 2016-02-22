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

import Control.Lens         hiding (cons)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List.Extra
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Game.Werewolf.Internal.Game     hiding (doesPlayerExist, getAllowedVoters,
                                                  getPassers, getPendingVoters, getPlayerVote,
                                                  getVoteResult, hasAngelWon, hasVillagersWon,
                                                  hasWerewolvesWon, killPlayer, setPlayerAllegiance)
import           Game.Werewolf.Internal.Messages
import           Game.Werewolf.Internal.Player
import           Game.Werewolf.Internal.Role     hiding (name)
import qualified Game.Werewolf.Internal.Role     as Role
import           Game.Werewolf.Internal.Util
import           Game.Werewolf.Response

import Prelude hiding (round)

checkStage :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkStage = do
    game <- get
    checkStage' >> checkEvents
    game' <- get

    when (game /= game') checkStage

checkStage' :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkStage' = use stage >>= \stage' -> case stage' of
    GameOver -> return ()

    DefendersTurn -> do
        whenM (is dead <$> findPlayerByRole_ defenderRole) advanceStage

        whenJustM (use protect) $ const advanceStage

    ScapegoatsTurn -> unlessM (use scapegoatBlamed) $ do
        allowedVoters' <- use allowedVoters
        tell [scapegoatChoseAllowedVotersMessage allowedVoters']

        advanceStage

    SeersTurn -> do
        seer <- findPlayerByRole_ seerRole

        when (is dead seer) advanceStage

        whenJustM (use see) $ \targetName -> do
            target <- findPlayerByName_ targetName

            tell [playerSeenMessage (seer ^. name) target]

            advanceStage

    Sunrise -> do
        round += 1

        whenJustM (preuse $ players . angels . alive) $ \angel -> do
            tell [angelJoinedVillagersMessage]

            setPlayerAllegiance (angel ^. name) Villagers

        advanceStage

    Sunset -> do
        whenJustM (use roleModel) $ \roleModelsName -> do
            wildChild <- findPlayerByRole_ wildChildRole

            whenM (isPlayerDead roleModelsName &&^ return (is villager wildChild)) $ do
                aliveWerewolfNames <- gets $ toListOf (players . werewolves . alive . name)

                setPlayerAllegiance (wildChild ^. name) Werewolves

                tell [playerJoinedPackMessage (wildChild ^. name) aliveWerewolfNames]
                tell $ wildChildJoinedPackMessages aliveWerewolfNames (wildChild ^. name)

        advanceStage

    UrsussGrunt -> do
        bearTamer   <- findPlayerByRole_ bearTamerRole
        players'    <- gets $ getAdjacentAlivePlayers (bearTamer ^. name)

        when (has werewolves players') $ tell [ursusGruntsMessage]

        advanceStage

    VillagesTurn -> whenM (null <$> liftM2 intersect getAllowedVoters getPendingVoters) $ do
        tell . map (uncurry playerMadeLynchVoteMessage) =<< uses votes Map.toList

        getVoteResult >>= lynchVotees

        allVoters       <- ifM (use villageIdiotRevealed)
            (uses players $ filter (isn't villageIdiot))
            (use players)
        allowedVoters   .= allVoters ^.. traverse . alive . name

        advanceStage

    WerewolvesTurn -> whenM (none (is werewolf) <$> getPendingVoters) $ do
        getVoteResult >>= devourVotees

        protect .= Nothing

        advanceStage

    WildChildsTurn -> do
        whenM (is dead <$> findPlayerByRole_ wildChildRole) advanceStage

        whenJustM (use roleModel) $ const advanceStage

    WitchsTurn -> do
        whenM (is dead <$> findPlayerByRole_ witchRole) advanceStage

        whenJustM (use poison) $ \targetName -> do
            events %= (++ [PoisonEvent targetName])
            poison .= Nothing

        whenM (use heal) $ do
            devourEvent <- uses events $ \events -> head [event | event@(DevourEvent _) <- events]

            events  %= cons NoDevourEvent . delete devourEvent
            heal    .= False

        whenM (use healUsed &&^ use poisonUsed) advanceStage
        whenM (has witches <$> getPassers)      advanceStage

    WolfHoundsTurn -> do
        whenM (is dead <$> findPlayerByRole_ wolfHoundRole) advanceStage

        whenJustM (use allegianceChosen) $ \allegiance -> do
            wolfHound <- findPlayerByRole_ wolfHoundRole

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
    passes  .= []
    see     .= Nothing
    votes   .= Map.empty

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
    player <- findPlayerByName_ targetName

    killPlayer targetName
    tell [playerDevouredMessage player]
applyEvent NoDevourEvent            = tell [noPlayerDevouredMessage]
applyEvent (PoisonEvent name)       = do
    player <- findPlayerByName_ name

    killPlayer name
    tell [playerPoisonedMessage player]

checkGameOver :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkGameOver = whenM hasAnyoneWon $ do
    stage .= GameOver

    tell . gameOverMessages =<< get

startGame :: (MonadError [Message] m, MonadWriter [Message] m) => Text -> [Player] -> m Game
startGame callerName players = do
    when (playerNames /= nub playerNames)   $ throwError [privateMessage callerName "Player names must be unique."]
    when (length players < 7)               $ throwError [privateMessage callerName "Must have at least 7 players."]
    when (length players > 24)              $ throwError [privateMessage callerName "Cannot have more than 24 players."]
    forM_ restrictedRoles $ \role' ->
        when (length (filter ((role' ==) . view role) players) > 1) $
            throwError [privateMessage callerName $ T.concat ["Cannot have more than 1 ", role' ^. Role.name, "."]]

    let game = newGame players

    tell $ newGameMessages game

    return game
    where
        playerNames = players ^.. names
