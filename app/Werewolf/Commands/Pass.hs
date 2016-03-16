{-|
Module      : Werewolf.Commands.Pass
Description : Handler for the pass subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Handler for the pass subcommand.
-}

module Werewolf.Commands.Pass (
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

handle :: MonadIO m => Text -> m ()
handle callerName = do
    unlessM doesGameExist $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame

    let command = case game ^. stage of
            DevotedServantsTurn -> passDevotedServantsTurnCommand callerName
            WitchsTurn          -> passWitchsTurnCommand callerName
            -- TODO (hjw): throw an error
            _                   -> undefined

    case runExcept (runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game', messages) -> writeGame game' >> exitWith success { messages = messages }
