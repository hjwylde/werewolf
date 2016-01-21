{-|
Module      : Werewolf.Commands.Vote
Description : Options and handler for the vote subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the vote subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Commands.Vote (
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
import Game.Werewolf.Engine   hiding (isVillagersTurn)
import Game.Werewolf.Game
import Game.Werewolf.Response

-- | Options.
data Options = Options
    { argTarget :: Text
    } deriving (Eq, Show)

-- | Handle.
handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options targetName) = do
    unlessM doesGameExist $ exitWith failure {
        messages = [privateMessage [callerName] "No game is running."]
        }

    game <- readGame

    let command = (if isVillagersTurn game
            then lynchVoteCommand
            else devourVoteCommand
            ) callerName targetName

    case runExcept (runWriterT $ execStateT (apply command >> checkTurn >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game', messages) -> writeGame game' >> exitWith success { messages = messages }
