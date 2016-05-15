{-|
Module      : Werewolf.Command.End
Description : Options and handler for the end subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the end subcommand.
-}

module Werewolf.Command.End (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Lens
import Control.Monad.Extra
import Control.Monad.IO.Class

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Message.Command
import Game.Werewolf.Message.Error

import Werewolf.System

data Options = Options
    { optForce :: Bool
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Text -> Options -> m ()
handle callerName tag (Options force) = do
    unlessM (doesGameExist tag) $ exitWith failure { messages = [noGameRunningMessage callerName] }

    unless force $ do
        game <- readGame tag

        unless (has (players . traverse . named callerName) game) $
            exitWith failure { messages = [playerCannotDoThatMessage callerName] }

    deleteGame tag

    exitWith success { messages = [gameEndedMessage callerName] }
