{-|
Module      : Werewolf.Command.Boot
Description : Options and handler for the boot subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the boot subcommand.
-}

module Werewolf.Command.Boot (
    -- * Options
    Options(..),

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
import Game.Werewolf.Command.Global

import Werewolf.Game
import Werewolf.Messages

data Options = Options
    { argTarget :: Text
    } deriving (Eq, Show)

handle :: (MonadIO m, MonadRandom m) => Text -> Text -> Options -> m ()
handle callerName tag (Options targetName) = do
    unlessM (doesGameExist tag) $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame tag

    let command = bootCommand callerName targetName

    result <- runExceptT . runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game
    case result of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game', messages) -> writeOrDeleteGame tag game' >> exitWith success { messages = messages }
