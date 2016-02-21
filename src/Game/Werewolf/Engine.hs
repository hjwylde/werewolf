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

    -- ** Manipulations
    startGame, killPlayer, setPlayerAllegiance,

    -- ** Searches
    findPlayerByName_, findPlayerByRole_,

    -- ** Queries
    isGameOver, isDefendersTurn, isScapegoatsTurn, isSeersTurn, isVillagesTurn, isWerewolvesTurn,
    isWildChildsTurn, isWitchsTurn, isWolfHoundsTurn,
    hasAngelWon, hasVillagersWon, hasWerewolvesWon,
    getPlayerVote, getAllowedVoters, getPendingVoters, getVoteResult,

    -- ** Reading and writing
    defaultFilePath, writeGame, readGame, deleteGame, doesGameExist,

    -- * Event

    -- ** Queries
    getDevourEvent,

    -- * Player

    -- ** Manipulations
    createPlayers,

    -- ** Queries
    doesPlayerExist,
    isPlayerDefender, isPlayerScapegoat, isPlayerSeer, isPlayerVillageIdiot, isPlayerWildChild,
    isPlayerWitch, isPlayerWolfHound,
    isPlayerWerewolf,
    isPlayerAlive, isPlayerDead,

    -- * Role
    padRoles,
) where

import Control.Lens         hiding (cons)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Random
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List.Extra
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Game.Werewolf.Internal.Game   hiding (doesPlayerExist, getAllowedVoters,
                                                getPassers, getPendingVoters, getPlayerVote,
                                                getVoteResult, hasAngelWon, hasVillagersWon,
                                                hasWerewolvesWon, killPlayer, setPlayerAllegiance)
import qualified Game.Werewolf.Internal.Game   as Game
import           Game.Werewolf.Internal.Player
import           Game.Werewolf.Internal.Role   hiding (name)
import qualified Game.Werewolf.Internal.Role   as Role
import           Game.Werewolf.Messages
import           Game.Werewolf.Response

import Prelude hiding (round)

import System.Directory
import System.FilePath
import System.Random.Shuffle

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

killPlayer :: MonadState Game m => Text -> m ()
killPlayer name = modify $ Game.killPlayer name

setPlayerAllegiance :: MonadState Game m => Text -> Allegiance -> m ()
setPlayerAllegiance name allegiance = modify $ Game.setPlayerAllegiance name allegiance

findPlayerByName_ :: MonadState Game m => Text -> m Player
findPlayerByName_ name' = fromJust <$> preuse (players . traverse . filteredBy name name')

findPlayerByRole_ :: MonadState Game m => Role -> m Player
findPlayerByRole_ role' = fromJust <$> preuse (players . traverse . filteredBy role role')

isDefendersTurn :: MonadState Game m => m Bool
isDefendersTurn = has (stage . _DefendersTurn) <$> get

isGameOver :: MonadState Game m => m Bool
isGameOver = has (stage . _GameOver) <$> get

isScapegoatsTurn :: MonadState Game m => m Bool
isScapegoatsTurn = has (stage . _ScapegoatsTurn) <$> get

isSeersTurn :: MonadState Game m => m Bool
isSeersTurn = has (stage . _SeersTurn) <$> get

isSunrise :: MonadState Game m => m Bool
isSunrise = has (stage . _Sunrise) <$> get

isVillagesTurn :: MonadState Game m => m Bool
isVillagesTurn = has (stage . _VillagesTurn) <$> get

isWerewolvesTurn :: MonadState Game m => m Bool
isWerewolvesTurn = has (stage . _WerewolvesTurn) <$> get

isWildChildsTurn :: MonadState Game m => m Bool
isWildChildsTurn = has (stage . _WildChildsTurn) <$> get

isWitchsTurn :: MonadState Game m => m Bool
isWitchsTurn = has (stage . _WitchsTurn) <$> get

isWolfHoundsTurn :: MonadState Game m => m Bool
isWolfHoundsTurn = has (stage . _WolfHoundsTurn) <$> get

hasAnyoneWon :: MonadState Game m => m Bool
hasAnyoneWon = hasAngelWon ||^ hasVillagersWon ||^ hasWerewolvesWon

hasAngelWon :: MonadState Game m => m Bool
hasAngelWon = gets Game.hasAngelWon

hasVillagersWon :: MonadState Game m => m Bool
hasVillagersWon = gets Game.hasVillagersWon

hasWerewolvesWon :: MonadState Game m => m Bool
hasWerewolvesWon = gets Game.hasWerewolvesWon

getPassers :: MonadState Game m => m [Player]
getPassers = gets Game.getPassers

getPlayerVote :: MonadState Game m => Text -> m (Maybe Text)
getPlayerVote playerName = gets $ Game.getPlayerVote playerName

getAllowedVoters :: MonadState Game m => m [Player]
getAllowedVoters = gets Game.getAllowedVoters

getPendingVoters :: MonadState Game m => m [Player]
getPendingVoters = gets Game.getPendingVoters

getVoteResult :: MonadState Game m => m [Player]
getVoteResult = gets Game.getVoteResult

defaultFilePath :: MonadIO m => m FilePath
defaultFilePath = (</> defaultFileName) <$> liftIO getHomeDirectory

defaultFileName :: FilePath
defaultFileName = ".werewolf"

readGame :: MonadIO m => m Game
readGame = liftIO . fmap read $ defaultFilePath >>= readFile

writeGame :: MonadIO m => Game -> m ()
writeGame game = liftIO $ defaultFilePath >>= flip writeFile (show game)

deleteGame :: MonadIO m => m ()
deleteGame = liftIO $ defaultFilePath >>= removeFile

doesGameExist :: MonadIO m => m Bool
doesGameExist = liftIO $ defaultFilePath >>= doesFileExist

getDevourEvent :: MonadState Game m => m (Maybe Text)
getDevourEvent = gets (^? events . traverse . _DevourEvent)

createPlayers :: MonadIO m => [Text] -> [Role] -> m [Player]
createPlayers playerNames roles = liftIO $ zipWith newPlayer playerNames <$> evalRandIO (shuffleM roles)

doesPlayerExist :: MonadState Game m => Text -> m Bool
doesPlayerExist name = gets $ Game.doesPlayerExist name

isPlayerDefender :: MonadState Game m => Text -> m Bool
isPlayerDefender name = is defender <$> findPlayerByName_ name

isPlayerScapegoat :: MonadState Game m => Text -> m Bool
isPlayerScapegoat name = is scapegoat <$> findPlayerByName_ name

isPlayerSeer :: MonadState Game m => Text -> m Bool
isPlayerSeer name = is seer <$> findPlayerByName_ name

isPlayerVillageIdiot :: MonadState Game m => Text -> m Bool
isPlayerVillageIdiot name = is villageIdiot <$> findPlayerByName_ name

isPlayerWildChild :: MonadState Game m => Text -> m Bool
isPlayerWildChild name = is wildChild <$> findPlayerByName_ name

isPlayerWitch :: MonadState Game m => Text -> m Bool
isPlayerWitch name = is witch <$> findPlayerByName_ name

isPlayerWolfHound :: MonadState Game m => Text -> m Bool
isPlayerWolfHound name = is wolfHound <$> findPlayerByName_ name

isPlayerWerewolf :: MonadState Game m => Text -> m Bool
isPlayerWerewolf name = is werewolf <$> findPlayerByName_ name

isPlayerAlive :: MonadState Game m => Text -> m Bool
isPlayerAlive name = is alive <$> findPlayerByName_ name

isPlayerDead :: MonadState Game m => Text -> m Bool
isPlayerDead name = is dead <$> findPlayerByName_ name

padRoles :: [Role] -> Int -> [Role]
padRoles roles n = roles ++ simpleVillagerRoles ++ simpleWerewolfRoles
    where
        goal                    = 3
        m                       = max (n - length roles) 0
        startingBalance         = sumOf (traverse . balance) roles
        simpleWerewolfBalance   = simpleWerewolfRole ^. balance

        -- Little magic here to calculate how many Werewolves and Villagers we want.
        -- This tries to ensure that the balance of the game is between -2 and 2.
        simpleWerewolvesCount   = (goal - m - startingBalance) `div` (simpleWerewolfBalance - 1) + 1
        simpleVillagersCount    = m - simpleWerewolvesCount

        -- N.B., if roles is quite unbalanced then one list will be empty.
        simpleVillagerRoles = replicate simpleVillagersCount simpleVillagerRole
        simpleWerewolfRoles = replicate simpleWerewolvesCount simpleWerewolfRole
