{-|
Module      : Game.Werewolf.Engine
Description : Engine functions.

Copyright   : (c) Henry J. Wylde, 2015
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
    startGame, killPlayer,

    -- ** Queries
    isGameOver, isDefendersTurn, isSeersTurn, isVillagesTurn, isWerewolvesTurn, isWitchsTurn,
    getPlayerVote, getPendingVoters, getVoteResult,

    -- ** Reading and writing
    defaultFilePath, writeGame, readGame, deleteGame, doesGameExist,

    -- * Event

    -- ** Queries
    getDevourEvent,

    -- * Player

    -- ** Manipulations
    createPlayers,

    -- ** Queries
    doesPlayerExist, isPlayerDefender, isPlayerSeer, isPlayerWerewolf, isPlayerWitch, isPlayerAlive,
    isPlayerDead,

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

import           Game.Werewolf.Game     hiding (getDevourEvent, getPassers, getPendingVoters,
                                         getPlayerVote, getVoteResult, isDefendersTurn, isGameOver,
                                         isSeersTurn, isVillagesTurn, isWerewolvesTurn,
                                         isWitchsTurn, killPlayer)
import qualified Game.Werewolf.Game     as Game
import           Game.Werewolf.Player   hiding (doesPlayerExist)
import qualified Game.Werewolf.Player   as Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role     hiding (name)
import qualified Game.Werewolf.Role     as Role

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

    DefendersTurn -> whenJustM (use protect) $ const advanceStage

    SeersTurn -> whenJustM (use see) $ \targetName -> do
        seer    <- uses players (head . filterSeers)
        target  <- uses players (findByName_ targetName)

        tell [playerSeenMessage (seer ^. name) target]

        advanceStage

    Sunrise -> advanceStage

    Sunset -> advanceStage

    VillagesTurn -> do
        playersCount    <- uses players (length . filterAlive)
        votes'          <- use votes

        when (playersCount == Map.size votes') $ do
            tell $ map (uncurry playerMadeLynchVoteMessage) (Map.toList votes')

            getVoteResult >>= \votees -> case votees of
                [votee]   -> do
                    killPlayer votee
                    tell [playerLynchedMessage votee]
                _               ->
                    uses players (filterAlive . filterScapegoats) >>= \aliveScapegoats -> case aliveScapegoats of
                        [scapegoat] -> killPlayer scapegoat >> tell [scapegoatLynchedMessage (scapegoat ^. name)]
                        _           -> tell [noPlayerLynchedMessage]

            advanceStage

    WerewolvesTurn -> do
        aliveWerewolves <- uses players (filterAlive . filterWerewolves)

        whenM (uses votes $ (length aliveWerewolves ==) . Map.size) $ do
            getVoteResult >>= \votees -> case votees of
                [target]    ->
                    ifM (uses protect $ maybe False (== target ^. name))
                        (events %= cons (ProtectEvent $ target ^. name))
                        (events %= cons (DevourEvent $ target ^. name))
                _           -> tell [noPlayerDevouredMessage]

            protect .= Nothing

            advanceStage

    WitchsTurn -> do
        whenJustM (use poison) $ \targetName -> do
            events %= (++ [PoisonEvent targetName])
            poison .= Nothing

        witch <- uses players (head . filterWitches)

        whenM (use healUsed &&^ use poisonUsed) advanceStage
        whenM (fmap (witch `elem`) getPassers)  advanceStage

advanceStage :: (MonadState Game m, MonadWriter [Message] m) => m ()
advanceStage = do
    game            <- get
    stage'          <- use stage
    alivePlayers    <- uses players filterAlive

    let nextStage = if length (nub $ map (view $ role . allegiance) alivePlayers) <= 1
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
eventAvailable (DevourEvent _) = gets isSunrise
eventAvailable (PoisonEvent _) = gets isSunrise
eventAvailable (ProtectEvent _) = gets isSunrise

applyEvent :: (MonadState Game m, MonadWriter [Message] m) => Event -> m ()
applyEvent (DevourEvent targetName) = do
    player  <- uses players $ findByName_ targetName
    heal'   <- use heal

    if heal'
        then tell [playerHealedMessage $ player ^. name]
        else do
            killPlayer player
            tell [playerDevouredMessage player]

    heal .= False
applyEvent (PoisonEvent name) = do
    player <- uses players $ findByName_ name

    killPlayer player

    tell [playerPoisonedMessage player]
applyEvent (ProtectEvent name) = tell [playerProtectedMessage name]

checkGameOver :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkGameOver = do
    aliveAllegiances <- uses players $ nub . map (view $ role . allegiance) . filterAlive

    when (length aliveAllegiances <= 1) $ stage .= GameOver >> get >>= tell . gameOverMessages

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
        restrictedRoles = [defenderRole, scapegoatRole, seerRole, villagerVillagerRole, witchRole]

killPlayer :: MonadState Game m => Player -> m ()
killPlayer player = players %= map (\player' -> if player' == player then player' & state .~ Dead else player')

isDefendersTurn :: MonadState Game m => m Bool
isDefendersTurn = gets Game.isDefendersTurn

isSeersTurn :: MonadState Game m => m Bool
isSeersTurn = gets Game.isSeersTurn

isVillagesTurn :: MonadState Game m => m Bool
isVillagesTurn = gets Game.isVillagesTurn

isWerewolvesTurn :: MonadState Game m => m Bool
isWerewolvesTurn = gets Game.isWerewolvesTurn

isWitchsTurn :: MonadState Game m => m Bool
isWitchsTurn = gets Game.isWitchsTurn

isGameOver :: MonadState Game m => m Bool
isGameOver = gets Game.isGameOver

getPassers :: MonadState Game m => m [Player]
getPassers = gets Game.getPassers

getPlayerVote :: MonadState Game m => Text -> m (Maybe Text)
getPlayerVote playerName = gets $ Game.getPlayerVote playerName

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
doesPlayerExist name = uses players $ Player.doesPlayerExist name

isPlayerDefender :: MonadState Game m => Text -> m Bool
isPlayerDefender name = uses players $ isDefender . findByName_ name

isPlayerSeer :: MonadState Game m => Text -> m Bool
isPlayerSeer name = uses players $ isSeer . findByName_ name

isPlayerWerewolf :: MonadState Game m => Text -> m Bool
isPlayerWerewolf name = uses players $ isWerewolf . findByName_ name

isPlayerWitch :: MonadState Game m => Text -> m Bool
isPlayerWitch name = uses players $ isWitch . findByName_ name

isPlayerAlive :: MonadState Game m => Text -> m Bool
isPlayerAlive name = uses players $ isAlive . findByName_ name

isPlayerDead :: MonadState Game m => Text -> m Bool
isPlayerDead name = uses players $ isDead . findByName_ name

randomiseRoles :: MonadIO m => [Role] -> Int -> m [Role]
randomiseRoles extraRoles n = liftIO . evalRandIO . shuffleM $ extraRoles ++ werewolfRoles ++ villagerRoles
    where
        extraWerewolfRoles = filter ((==) Role.Werewolves . view allegiance) extraRoles
        extraVillagerRoles = filter ((==) Role.Villagers . view allegiance) extraRoles

        werewolfRoles = replicate (n `quot` 6 + 1 - length extraWerewolfRoles) werewolfRole
        villagerRoles = replicate (n - length (extraVillagerRoles ++ werewolfRoles)) villagerRole
