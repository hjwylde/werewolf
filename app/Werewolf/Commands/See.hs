{-|
Module      : Werewolf.Commands.See
Description : Options and handler for the see subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the see subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Commands.See (
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

import Game.Werewolf.Command
import Game.Werewolf.Engine
import Game.Werewolf.Response

-- | Options.
data Options = Options
    { argTarget :: Text
    } deriving (Eq, Show)

-- | Handle.
handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options targetName) = do
    unlessM doesGameExist $ exitWith failure { messages = [privateMessage [callerName] "No game is running."] }

    game <- readGame

    let command = seeCommand callerName targetName

    case runExcept (runWriterT $ execStateT (apply command >> advanceTurn >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game', messages) -> writeGame game' >> exitWith success { messages = messages }
