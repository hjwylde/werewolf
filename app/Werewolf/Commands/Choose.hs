{-|
Module      : Werewolf.Commands.Choose
Description : Options and handler for the choose subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the choose subcommand.
-}

module Werewolf.Commands.Choose (
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
import Game.Werewolf.Engine   hiding (isWildChildsTurn)
import Game.Werewolf.Game
import Game.Werewolf.Response

data Options = Options
    { arg :: Text
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options arg) = do
    unlessM doesGameExist $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame

    let command = (if isWildChildsTurn game
            then choosePlayerCommand
            else chooseAllegianceCommand
            ) callerName arg

    case runExcept (runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeGame game >> exitWith success { messages = messages }
