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
    startGame, killPlayer, setPlayerRole,

    -- ** Searches
    findPlayerByName_, findPlayerByRole_,

    -- ** Queries
    isGameOver, isDefendersTurn, isScapegoatsTurn, isSeersTurn, isVillagesTurn, isWerewolvesTurn,
    isWildChildsTurn, isWitchsTurn, isWolfHoundsTurn,
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
    randomiseRoles,
) where

import Control.Lens         hiding (cons, snoc)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Random
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List.Extra
import qualified Data.Map        as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Game.Werewolf.Game            hiding (doesPlayerExist, getAllowedVoters,
                                                getDevourEvent, getPassers, getPendingVoters,
                                                getPlayerVote, getVoteResult, isDefendersTurn,
                                                isGameOver, isScapegoatsTurn, isSeersTurn,
                                                isVillagesTurn, isWerewolvesTurn, isWildChildsTurn,
                                                isWitchsTurn, isWolfHoundsTurn, killPlayer,
                                                setPlayerAllegiance, setPlayerRole)
import qualified Game.Werewolf.Game            as Game
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
        whenM (isDead <$> findPlayerByRole_ defenderRole) advanceStage

        whenJustM (use protect) $ const advanceStage

    ScapegoatsTurn -> unlessM (use scapegoatBlamed) $ do
        allowedVoters' <- use allowedVoters
        tell [scapegoatChoseAllowedVotersMessage allowedVoters']

        advanceStage

    SeersTurn -> do
        seer <- findPlayerByRole_ seerRole

        when (isDead seer) advanceStage

        whenJustM (use see) $ \targetName -> do
            target <- findPlayerByName_ targetName

            tell [playerSeenMessage (seer ^. name) target]

            advanceStage

    Sunrise -> do
        round += 1

        whenJustM (findAlivePlayerByRole angelRole) $ \angel -> do
            tell [angelJoinedVillagersMessage]

            setPlayerRole (angel ^. name) simpleVillagerRole

        advanceStage

    Sunset -> do
        whenJustM (use roleModel) $ \roleModelsName -> do
            wildChild <- findPlayerByRole_ wildChildRole

            whenM (isPlayerDead roleModelsName &&^ return (isVillager wildChild)) $ do
                aliveWerewolfNames <- uses players (map (view name) . filterAlive . filterWerewolves)

                setPlayerAllegiance (wildChild ^. name) Werewolves

                tell [playerJoinedPackMessage (wildChild ^. name) aliveWerewolfNames]
                tell $ wildChildJoinedPackMessages aliveWerewolfNames (wildChild ^. name)

        advanceStage

    VillagesTurn -> whenM (null <$> liftM2 intersect getAllowedVoters getPendingVoters) $ do
        tell . map (uncurry playerMadeLynchVoteMessage) =<< uses votes Map.toList

        getVoteResult >>= lynchVotees

        allowedVoters'  <- ifM (use villageIdiotRevealed)
            (uses players (filter $ not . isVillageIdiot))
            (use players)
        allowedVoters   .= map (view name) (filterAlive allowedVoters')

        advanceStage

    WerewolvesTurn -> whenM (null . filterWerewolves <$> getPendingVoters) $ do
        getVoteResult >>= devourVotees

        protect .= Nothing

        advanceStage

    WildChildsTurn -> do
        whenM (isDead <$> findPlayerByRole_ wildChildRole) advanceStage

        whenJustM (use roleModel) $ const advanceStage

    WitchsTurn -> do
        whenM (isDead <$> findPlayerByRole_ witchRole) advanceStage

        whenJustM (use poison) $ \targetName -> do
            events %= (++ [PoisonEvent targetName])
            poison .= Nothing

        whenM (use heal) $ do
            devourEvent <- uses events $ \events -> head [event | event@(DevourEvent _) <- events]

            events  %= cons NoDevourEvent . delete devourEvent
            heal    .= False

        whenM (use healUsed &&^ use poisonUsed) advanceStage
        whenM (any isWitch <$> getPassers)      advanceStage

    WolfHoundsTurn -> unlessM (uses players (any isWolfHound . filterAlive)) advanceStage

lynchVotees :: (MonadState Game m, MonadWriter [Message] m) => [Player] -> m ()
lynchVotees [votee]
    | isVillageIdiot votee  = do
        villageIdiotRevealed .= True

        tell [villageIdiotLynchedMessage $ votee ^. name]
    | otherwise             = do
        killPlayer (votee ^. name)
        tell [playerLynchedMessage votee]
lynchVotees _       = findAlivePlayerByRole scapegoatRole >>= \mScapegoat -> case mScapegoat of
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
    game                <- get
    stage'              <- use stage
    aliveAllegiances    <- uses players (nub . map (view $ role . allegiance) . filterAlive)

    let nextStage = if length aliveAllegiances <= 1 || any isAngel (filterDead $ game ^. players)
        then GameOver
        else head $ filter (stageAvailable game) (drop1 $ dropWhile (stage' /=) stageCycle)

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
eventAvailable (DevourEvent _)  = gets isSunrise
eventAvailable NoDevourEvent    = gets isSunrise
eventAvailable (PoisonEvent _)  = gets isSunrise

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
checkGameOver = do
    aliveAllegiances    <- uses players (nub . map (view $ role . allegiance) . filterAlive)
    deadPlayers         <- uses players filterDead

    when (length aliveAllegiances <= 1 || any isAngel deadPlayers) $ do
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
        playerNames = map (view name) players

killPlayer :: MonadState Game m => Text -> m ()
killPlayer name = modify $ Game.killPlayer name

setPlayerRole :: MonadState Game m => Text -> Role -> m ()
setPlayerRole name role = modify $ Game.setPlayerRole name role

setPlayerAllegiance :: MonadState Game m => Text -> Allegiance -> m ()
setPlayerAllegiance name allegiance = modify $ Game.setPlayerAllegiance name allegiance

findPlayerByName_ :: MonadState Game m => Text -> m Player
findPlayerByName_ name = uses players $ findByName_ name

findPlayerByRole_ :: MonadState Game m => Role -> m Player
findPlayerByRole_ role = uses players $ findByRole_ role

findAlivePlayerByRole :: MonadState Game m => Role -> m (Maybe Player)
findAlivePlayerByRole role = uses players $ findByRole role . filterAlive

isDefendersTurn :: MonadState Game m => m Bool
isDefendersTurn = gets Game.isDefendersTurn

isScapegoatsTurn :: MonadState Game m => m Bool
isScapegoatsTurn = gets Game.isScapegoatsTurn

isSeersTurn :: MonadState Game m => m Bool
isSeersTurn = gets Game.isSeersTurn

isVillagesTurn :: MonadState Game m => m Bool
isVillagesTurn = gets Game.isVillagesTurn

isWerewolvesTurn :: MonadState Game m => m Bool
isWerewolvesTurn = gets Game.isWerewolvesTurn

isWildChildsTurn :: MonadState Game m => m Bool
isWildChildsTurn = gets Game.isWildChildsTurn

isWitchsTurn :: MonadState Game m => m Bool
isWitchsTurn = gets Game.isWitchsTurn

isWolfHoundsTurn :: MonadState Game m => m Bool
isWolfHoundsTurn = gets Game.isWolfHoundsTurn

isGameOver :: MonadState Game m => m Bool
isGameOver = gets Game.isGameOver

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

getDevourEvent :: MonadState Game m => m (Maybe Event)
getDevourEvent = gets Game.getDevourEvent

createPlayers :: MonadIO m => [Text] -> [Role] -> m [Player]
createPlayers playerNames extraRoles = zipWith newPlayer playerNames <$> randomiseRoles extraRoles (length playerNames)

doesPlayerExist :: MonadState Game m => Text -> m Bool
doesPlayerExist name = gets $ Game.doesPlayerExist name

isPlayerDefender :: MonadState Game m => Text -> m Bool
isPlayerDefender name = isDefender <$> findPlayerByName_ name

isPlayerScapegoat :: MonadState Game m => Text -> m Bool
isPlayerScapegoat name = isScapegoat <$> findPlayerByName_ name

isPlayerSeer :: MonadState Game m => Text -> m Bool
isPlayerSeer name = isSeer <$> findPlayerByName_ name

isPlayerVillageIdiot :: MonadState Game m => Text -> m Bool
isPlayerVillageIdiot name = isVillageIdiot <$> findPlayerByName_ name

isPlayerWildChild :: MonadState Game m => Text -> m Bool
isPlayerWildChild name = isWildChild <$> findPlayerByName_ name

isPlayerWitch :: MonadState Game m => Text -> m Bool
isPlayerWitch name = isWitch <$> findPlayerByName_ name

isPlayerWolfHound :: MonadState Game m => Text -> m Bool
isPlayerWolfHound name = isWolfHound <$> findPlayerByName_ name

isPlayerWerewolf :: MonadState Game m => Text -> m Bool
isPlayerWerewolf name = isWerewolf <$> findPlayerByName_ name

isPlayerAlive :: MonadState Game m => Text -> m Bool
isPlayerAlive name = isAlive <$> findPlayerByName_ name

isPlayerDead :: MonadState Game m => Text -> m Bool
isPlayerDead name = isDead <$> findPlayerByName_ name

randomiseRoles :: MonadIO m => [Role] -> Int -> m [Role]
randomiseRoles extraRoles n = liftIO . evalRandIO . shuffleM $ extraRoles ++ simpleVillagerRoles ++ simpleWerewolfRoles
    where
        goal                    = 2
        m                       = max (n - length extraRoles) 0
        startingBalance         = sum (map (view balance) extraRoles)
        simpleWerewolfBalance   = simpleWerewolfRole ^. balance

        -- Little magic here to calculate how many Werewolves and Villagers we want.
        -- Essentially we get a 1:4 ratio of Werewolves to Villagers.
        simpleWerewolvesCount   = (goal - m - startingBalance) `div` (simpleWerewolfBalance - 1) + 1
        simpleVillagersCount    = m - simpleWerewolvesCount

        -- N.B., if extraRoles is quite unbalanced then one list will be empty while the other will
        -- be full. This then leaves it up to the magic of shuffle to try rebalance the roles.
        simpleWerewolfRoles = replicate simpleWerewolvesCount simpleWerewolfRole
        simpleVillagerRoles = replicate simpleVillagersCount simpleVillagerRole
