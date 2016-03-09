{-|
Module      : Werewolf.Commands.Choose
Description : Options and handler for the choose subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the choose subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Commands.Choose (
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

import Data.Text (Text)

import Game.Werewolf

import Werewolf.Game
import Werewolf.Messages

data Options = Options
    { args :: [Text]
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options args) = do
    unlessM doesGameExist $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame

    let command = case game ^. stage of
            ScapegoatsTurn  -> choosePlayersCommand callerName args
            WildChildsTurn  -> choosePlayerCommand callerName (head args)
            WolfHoundsTurn  -> chooseAllegianceCommand callerName (head args)
            -- TODO (hjw): throw an error
            _               -> undefined

    case runExcept (runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeGame game >> exitWith success { messages = messages }
