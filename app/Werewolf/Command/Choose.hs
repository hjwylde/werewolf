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
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Command.Hunter    as Hunter
import Game.Werewolf.Command.Orphan    as Orphan
import Game.Werewolf.Command.Scapegoat as Scapegoat
import Game.Werewolf.Engine
import Game.Werewolf.Message.Error

import Werewolf.System

data Options = Options
    { args :: [Text]
    } deriving (Eq, Show)

handle :: (MonadIO m, MonadRandom m) => Text -> Text -> Options -> m ()
handle callerName tag (Options args) = do
    unlessM (doesGameExist tag) $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame tag

    command <- case game ^. stage of
            HuntersTurn1    -> return $ Hunter.chooseCommand callerName (head args)
            HuntersTurn2    -> return $ Hunter.chooseCommand callerName (head args)
            OrphansTurn     -> return $ Orphan.chooseCommand callerName (head args)
            ScapegoatsTurn  -> return $ Scapegoat.chooseCommand callerName args
            _               -> exitWith failure
                { messages = [playerCannotDoThatRightNowMessage callerName]
                }

    result <- runExceptT . runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game
    case result of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeOrDeleteGame tag game >> exitWith success { messages = messages }
