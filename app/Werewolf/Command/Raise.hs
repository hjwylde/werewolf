{-|
Module      : Werewolf.Command.Raise
Description : Handler for the raise subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Handler for the raise subcommand.
-}

module Werewolf.Command.Raise (
    -- * Handle
    handle,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Command.Necromancer
import Game.Werewolf.Engine
import Game.Werewolf.Message.Error

import Werewolf.System

handle :: (MonadIO m, MonadRandom m) => Text -> Text -> m ()
handle callerName tag = do
    unlessM (doesGameExist tag) $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame tag

    let command = raiseCommand callerName

    result <- runExceptT . runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game
    case result of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game', messages) -> writeOrDeleteGame tag game' >> exitWith success { messages = messages }
