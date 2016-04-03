{-|
Module      : Werewolf.Command.Pass
Description : Handler for the pass subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Handler for the pass subcommand.
-}

module Werewolf.Command.Pass (
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
import Game.Werewolf.Command.DevotedServant as DevotedServant
import Game.Werewolf.Command.Witch          as Witch

import Werewolf.Game
import Werewolf.Messages

handle :: MonadIO m => Text -> Text -> m ()
handle callerName tag = do
    unlessM (doesGameExist tag) $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame tag

    command <- case game ^. stage of
            DevotedServantsTurn -> return $ DevotedServant.passCommand callerName
            WitchsTurn          -> return $ Witch.passCommand callerName
            _                   -> exitWith failure
                { messages = [playerCannotDoThatRightNowMessage callerName]
                }

    case runExcept (runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game', messages) -> writeOrDeleteGame tag game' >> exitWith success { messages = messages }
