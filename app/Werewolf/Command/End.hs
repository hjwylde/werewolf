{-|
Module      : Werewolf.Command.End
Description : Handler for the end subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Handler for the end subcommand.
-}

module Werewolf.Command.End (
    -- * Handle
    handle,
) where

import Control.Lens
import Control.Monad.Extra
import Control.Monad.IO.Class

import Data.Text (Text)

-- TODO (hjw): remove Message.Engine
import Game.Werewolf
import Game.Werewolf.Message.Command
import Game.Werewolf.Message.Error
import Game.Werewolf.Message.Engine

import Werewolf.System

handle :: MonadIO m => Text -> Text -> m ()
handle callerName tag = do
    unlessM (doesGameExist tag) $ exitWith failure { messages = [noGameRunningMessage callerName] }

    game <- readGame tag

    unless (has (players . traverse . named callerName) game) $
        exitWith failure { messages = [playerCannotDoThatMessage callerName] }

    deleteGame tag

    exitWith success { messages =
        [ gameEndedMessage callerName
        , playerRolesMessage game
        ] }
