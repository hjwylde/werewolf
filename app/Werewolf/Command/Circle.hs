{-|
Module      : Werewolf.Command.Circle
Description : Options and handler for the circle subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the circle subcommand.
-}

module Werewolf.Command.Circle (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Command.Status
import Game.Werewolf.Messages

import Werewolf.System

data Options = Options
    { optIncludeDead :: Bool
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Text -> Options -> m ()
handle callerName tag (Options includeDead) = do
    unlessM (doesGameExist tag) $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame tag

    let command = circleCommand callerName includeDead

    case runExcept . execWriterT $ execStateT (apply command) game of
        Left errorMessages  -> exitWith failure { messages = errorMessages }
        Right messages      -> exitWith success { messages = messages }
