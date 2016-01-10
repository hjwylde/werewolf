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
    -- * Loop
    checkGameOver,

    -- * Game

    -- ** Manipulating
    startGame, isSeersTurn, isVillagersTurn, isWerewolvesTurn, isGameOver, getPlayerSee,
    getPlayerVote,

    -- ** Reading and writing
    defaultFilePath, writeGame, readGame, deleteGame, doesGameExist,

    -- * Player
    createPlayers, doesPlayerExist, killPlayer,

    -- * Role
    randomiseRoles,
) where

import Control.Lens         hiding (only)
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.Aeson           hiding ((.=))
import qualified Data.ByteString.Lazy as BS
import           Data.List.Extra
import           Data.Text            (Text)

import           Game.Werewolf.Game     hiding (isGameOver, isSeersTurn, isVillagersTurn,
                                         isWerewolvesTurn)
import qualified Game.Werewolf.Game     as Game
import           Game.Werewolf.Player   hiding (doesPlayerExist)
import qualified Game.Werewolf.Player   as Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role     as Role

import System.Directory
import System.Exit
import System.FilePath
import System.Random.Shuffle

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

isSeersTurn :: MonadState Game m => m Bool
isSeersTurn = gets Game.isSeersTurn

isVillagersTurn :: MonadState Game m => m Bool
isVillagersTurn = gets Game.isVillagersTurn

isWerewolvesTurn :: MonadState Game m => m Bool
isWerewolvesTurn = gets Game.isWerewolvesTurn

isGameOver :: MonadState Game m => m Bool
isGameOver = gets Game.isGameOver

getPlayerSee :: MonadState Game m => Player -> m (Maybe Text)
getPlayerSee player = use $ turn . sees . at (player ^. Player.name)

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
randomiseRoles n = liftIO . evalRandIO . shuffleM $ seerRoles ++ werewolfRoles ++ villagerRoles
    where
        seerRoles       = [seerRole]
        werewolfRoles   = replicate (n `quot` 6 + 1) werewolfRole
        villagerRoles   = replicate (n - length (seerRoles ++ werewolfRoles)) villagerRole
