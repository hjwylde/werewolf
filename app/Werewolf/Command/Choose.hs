{-|
Module      : Werewolf.Command.Choose
Description : Options and handler for the choose subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the choose subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Command.Choose (
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
import Game.Werewolf.Command.Scapegoat as Scapegoat
import Game.Werewolf.Command.WildChild as WildChild
import Game.Werewolf.Command.WolfHound as WolfHound

import Werewolf.Game
import Werewolf.Messages

data Options = Options
    { args :: [Text]
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Text -> Options -> m ()
handle callerName tag (Options args) = do
    unlessM (doesGameExist tag) $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame tag

    let command = case game ^. stage of
            ScapegoatsTurn  -> Scapegoat.chooseCommand callerName args
            WildChildsTurn  -> WildChild.chooseCommand callerName (head args)
            WolfHoundsTurn  -> WolfHound.chooseCommand callerName (head args)
            -- TODO (hjw): throw an error
            _               -> undefined

    case runExcept (runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeGame tag game >> exitWith success { messages = messages }
