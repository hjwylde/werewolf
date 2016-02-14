{-|
Module      : Werewolf.Commands.Version
Description : Handler for the version subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Handler for the start subcommand.
-}

module Werewolf.Commands.Version (
    -- * Handle
    handle,
) where

import Control.Monad.Except

import Data.Text (Text)

import Game.Werewolf.Response

import Werewolf.Version

-- | Handle.
handle :: MonadIO m => Text -> m ()
handle callerName = exitWith success { messages = [engineVersionMessage callerName version] }
