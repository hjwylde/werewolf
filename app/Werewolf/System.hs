{-|
Module      : Werewolf.System
Description : System functions for working with a game state file.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

This module defines a few system functions for working with a game state file.
-}

module Werewolf.System (
    -- * Game

    -- ** Creating anew
    startGame,

    -- ** Working with an existing
    filePath, readGame, writeGame, deleteGame, writeOrDeleteGame, doesGameExist,
) where

import Control.Lens         hiding (cons)
import Control.Monad.Except

import           Data.Text (Text)
import qualified Data.Text as T

import Game.Werewolf

import Prelude hiding (round)

import System.Directory
import System.FilePath

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

writeOrDeleteGame :: MonadIO m => Text -> Game -> m ()
writeOrDeleteGame tag game
    | has (stage . _GameOver) game  = deleteGame tag
    | otherwise                     = writeGame tag game

doesGameExist :: MonadIO m => Text -> m Bool
doesGameExist tag = liftIO $ filePath tag >>= doesFileExist
