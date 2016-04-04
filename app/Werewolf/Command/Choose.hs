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
import Game.Werewolf.Command.Orphan    as Orphan
import Game.Werewolf.Command.Scapegoat as Scapegoat
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

    command <- case game ^. stage of
            OrphansTurn     -> return $ Orphan.chooseCommand callerName (head args)
            ScapegoatsTurn  -> return $ Scapegoat.chooseCommand callerName args
            WolfHoundsTurn  -> return $ WolfHound.chooseCommand callerName (head args)
            _               -> exitWith failure
                { messages = [playerCannotDoThatRightNowMessage callerName]
                }

    case runExcept (runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeOrDeleteGame tag game >> exitWith success { messages = messages }
