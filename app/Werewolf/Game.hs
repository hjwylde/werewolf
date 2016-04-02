{-|
Module      : Werewolf.Game
Description : Game functions pertaining to binary calls.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

This module defines a few utility functions for working with a game state file. It also provides
functions for padding roles and shuffling them when creating players.
-}

module Werewolf.Game (
    -- * Game

    -- ** Creating anew
    startGame, createPlayers, padRoles,

    -- ** Working with an existing
    filePath, readGame, writeGame, deleteGame, doesGameExist,
) where

import Control.Lens         hiding (cons)
import Control.Monad.Except
import Control.Monad.Random

import Data.List.Extra
import Data.Text       (Text)
import qualified Data.Text       as T

import Game.Werewolf

import Prelude hiding (round)

import System.Directory
import System.FilePath
import System.Random.Shuffle

createPlayers :: MonadIO m => [Text] -> [Role] -> m [Player]
createPlayers playerNames roles = liftIO $ zipWith newPlayer playerNames <$> evalRandIO (shuffleM roles)

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

filePath :: MonadIO m => Text -> m FilePath
filePath tag = (</> ".werewolf" </> T.unpack tag) <$> liftIO getHomeDirectory

readGame :: MonadIO m => Text -> m Game
readGame tag = liftIO . fmap read $ filePath tag >>= readFile

writeGame :: MonadIO m => Text -> Game -> m ()
writeGame tag game = liftIO $ filePath tag >>= \tag -> do
    createDirectoryIfMissing True (dropFileName tag)

    writeFile tag (show game)

deleteGame :: MonadIO m => Text -> m ()
deleteGame tag = liftIO $ filePath tag >>= removeFile

doesGameExist :: MonadIO m => Text -> m Bool
doesGameExist tag = liftIO $ filePath tag >>= doesFileExist
