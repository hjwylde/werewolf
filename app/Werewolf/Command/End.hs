{-|
Module      : Werewolf.Command.End
Description : Options and handler for the end subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the end subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Command.End (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Extra
import Control.Monad.IO.Class

import           Data.Text (Text)
import qualified Data.Text as T

import Game.Werewolf

import Werewolf.Game
import Werewolf.Messages

data Options = Options
    { optForce :: Bool
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options force) = do
    unlessM doesGameExist $ exitWith failure { messages = [noGameRunningMessage callerName] }

    unless force $ do
        game <- readGame

        unless (doesPlayerExist callerName game) $
            exitWith failure { messages = [playerCannotDoThatMessage callerName] }

    deleteGame

    exitWith success { messages = [gameEndedMessage] }
    where
        gameEndedMessage = publicMessage $ T.concat ["Game ended by ", callerName, "."]
