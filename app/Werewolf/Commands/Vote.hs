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

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import Data.Text  (Text)

import Game.Werewolf.Command
import Game.Werewolf.Engine
import Game.Werewolf.Game
import Game.Werewolf.Player   as Player
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

    let mCaller = findByName callerName (game ^. players)
    let mTarget = findByName targetName (game ^. players)

    when (isNothing mCaller) $ exitWith failure { messages = [playerDoesNotExistMessage callerName callerName] }
    when (isNothing mTarget) $ exitWith failure { messages = [playerDoesNotExistMessage callerName targetName] }

    let command = Vote { voter = fromJust mCaller, target = fromJust mTarget }

    case runExcept (runWriterT $ execStateT (validateCommand command >> applyCommand command >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game', messages) -> writeGame game' >> exitWith success { messages = messages }
