{-|
Module      : Werewolf.Commands.Ping
Description : Handler for the ping subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Handler for the ping subcommand.
-}

module Werewolf.Commands.Ping (
    -- * Handle
    handle,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import Data.Text (Text)

import Game.Werewolf

import Werewolf.Game
import Werewolf.Messages

handle :: MonadIO m => Text -> m ()
handle callerName = do
    unlessM doesGameExist $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame

    let command = pingCommand callerName

    case runExcept (execWriterT $ execStateT (apply command) game) of
        Left errorMessages  -> exitWith failure { messages = errorMessages }
        Right messages      -> exitWith success { messages = messages }
