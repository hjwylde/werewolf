{-|
Module      : Game.Werewolf.Engine
Description : Engine functions.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Engine functions.
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Game.Werewolf.Engine (
    -- * Command
    validateCommand, applyCommand, checkGameOver,

    -- * Game

    -- ** Manipulating
    startGame, isGameOver, getPlayerVote,

    -- ** Reading and writing
    defaultFilePath, writeGame, readGame, deleteGame, doesGameExist,

    -- * Player
    createPlayers, doesPlayerExist,

    -- * Role
    randomiseRoles,
) where

import Control.Lens hiding (only)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Random
import Control.Monad.Writer
import Control.Monad.State hiding (state)

import           Data.Aeson hiding ((.=))
import           Data.List.Extra
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)

import Game.Werewolf.Game hiding (isGameOver)
import qualified Game.Werewolf.Game as Game
import Game.Werewolf.Command
import qualified Game.Werewolf.Player   as Player
import Game.Werewolf.Player   hiding (doesPlayerExist)
import Game.Werewolf.Response
import Game.Werewolf.Role     as Role

import System.Directory
import System.Exit
import System.FilePath
import System.Random.Shuffle

validateCommand :: MonadError [Message] m => MonadState Game m => Command -> m ()
validateCommand (Vote voter target) = do
    whenM isGameOver                        $ throwError [gameIsOverMessage voterName]
    unlessM (doesPlayerExist voterName)     $ throwError [playerDoesNotExistMessage voterName voterName]
    unlessM (doesPlayerExist targetName)    $ throwError [playerDoesNotExistMessage voterName targetName]

    when (isDead voter)     $ throwError [playerIsDeadMessage voterName]
    when (isDead target)    $ throwError [targetIsDeadMessage voterName targetName]

    whenJustM (getPlayerVote voter) $ const (throwError [playerHasAlreadyVotedMessage voterName])

    get >>= \game -> when (isWerewolvesTurn game && not (isWerewolf voter)) $ throwError [playerCannotDoThatMessage voterName]
    where
        voterName = voter ^. Player.name
        targetName = target ^. Player.name

applyCommand :: (MonadError [Message] m, MonadState Game m, MonadWriter [Message] m) => Command -> m ()
applyCommand (Vote voter target) = do
    turn . votes %= Map.insert voterName targetName

    use turn >>= \turn' -> case turn' of
        Villagers {}    -> applyLynchVote
        Werewolves {}   -> applyKillVote
        NoOne           -> throwError [gameIsOverMessage voterName]
    where
        voterName   = voter ^. Player.name
        targetName  = target ^. Player.name

applyKillVote :: (MonadState Game m, MonadWriter [Message] m) => m ()
applyKillVote = do
    werewolvesCount <- uses players (length . filterAlive . filterWerewolves)
    votes           <- use $ turn . votes

    when (werewolvesCount == Map.size votes) $ do
        werewolfNames <- uses players (map Player._name . filterWerewolves)
        tell $ map (uncurry $ playerMadeKillVoteMessage werewolfNames) (Map.toList votes)

        turn .= newVillagersTurn
        tell villagersTurnMessages

        let mTargetName = only . last $ groupSortOn (length . flip elemIndices (Map.elems votes)) (nub $ Map.elems votes)
        case mTargetName of
            Nothing         -> tell [noPlayerKilledMessage]
            Just targetName -> do
                target <- uses players (findByName_ targetName)

                killPlayer target
                tell [playerKilledMessage (target ^. Player.name) (target ^. Player.role . Role.name)]

applyLynchVote :: (MonadState Game m, MonadWriter [Message] m) => m ()
applyLynchVote = do
    playersCount    <- uses players (length . filterAlive)
    votes           <- use $ turn . votes

    when (playersCount == Map.size votes) $ do
        tell $ map (uncurry playerMadeLynchVoteMessage) (Map.toList votes)

        let mLynchedName = only . last $ groupSortOn (length . flip elemIndices (Map.elems votes)) (nub $ Map.elems votes)
        case mLynchedName of
            Nothing             -> tell [noPlayerLynchedMessage]
            Just lynchedName    -> do
                target <- uses players (findByName_ lynchedName)

                killPlayer target
                tell [playerLynchedMessage (target ^. Player.name) (target ^. Player.role . Role.name)]

        turn .= newWerewolvesTurn
        tell werewolvesTurnMessages

only :: [a] -> Maybe a
only [a]    = Just a
only _      = Nothing

checkGameOver :: (MonadState Game m, MonadWriter [Message] m) => m ()
checkGameOver = do
    alivePlayers <- uses players filterAlive

    case length alivePlayers of
        0 -> turn .= NoOne >> tell [gameOverMessage Nothing]
        1 -> turn .= NoOne >> tell [gameOverMessage . Just $ head alivePlayers ^. Player.role . Role.name]
        _ -> return ()

startGame :: MonadError [Message] m => Text -> [Player] -> m Game
startGame callerName players = do
    when (playerNames /= nub playerNames)   $ throwError [privateMessage [callerName] "Player names must be unique."]
    when (length players < 7)               $ throwError [privateMessage [callerName] "Must have at least 7 players."]
    when (length players > 24)              $ throwError [privateMessage [callerName] "Cannot have more than 24 players."]

    return $ newGame players
    where
        playerNames = map Player._name players

isGameOver :: MonadState Game m => m Bool
isGameOver = gets Game.isGameOver

getPlayerVote :: MonadState Game m => Player -> m (Maybe Text)
getPlayerVote player = use $ turn . votes . at (player ^. Player.name)

defaultFilePath :: MonadIO m => m FilePath
defaultFilePath = (</> defaultFileName) <$> liftIO getHomeDirectory

defaultFileName :: FilePath
defaultFileName = ".werewolf"

readGame :: MonadIO m => m Game
readGame = liftIO $ defaultFilePath >>= BS.readFile >>= either die return . eitherDecode

writeGame :: MonadIO m => Game -> m ()
writeGame game = defaultFilePath >>= liftIO . flip BS.writeFile (encode game)

deleteGame :: MonadIO m => m ()
deleteGame = liftIO $ defaultFilePath >>= removeFile

doesGameExist :: MonadIO m => m Bool
doesGameExist = liftIO $ defaultFilePath >>= doesFileExist

createPlayers :: MonadIO m => [Text] -> m [Player]
createPlayers playerNames = zipWith newPlayer playerNames <$> randomiseRoles (length playerNames)

doesPlayerExist :: MonadState Game m => Text -> m Bool
doesPlayerExist name = uses players $ Player.doesPlayerExist name

killPlayer :: MonadState Game m => Player -> m ()
killPlayer player = players %= map (\player' -> if player' == player then player' & state .~ Dead else player')

randomiseRoles :: MonadIO m => Int -> m [Role]
randomiseRoles n = liftIO . evalRandIO . shuffleM $ werewolfRoles ++ villagerRoles
    where
        werewolfRoles = replicate (n `quot` 6 + 1) werewolf
        villagerRoles = replicate (n - length werewolfRoles) villager
