{-|
Module      : Game.Werewolf.Command.Global
Description : Global commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Global commands.
-}

module Game.Werewolf.Command.Global (
    -- * Commands
    bootCommand, quitCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Writer

import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import Game.Werewolf
import Game.Werewolf.Messages
import Game.Werewolf.Util

bootCommand :: Text -> Text -> Command
bootCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    validatePlayer callerName targetName
    whenM (uses (boots . at targetName) $ elem callerName . fromMaybe []) $
        throwError [playerHasAlreadyVotedToBootMessage callerName targetName]

    boots %= Map.insertWith (++) targetName [callerName]

    tell [playerVotedToBootMessage callerName targetName]

quitCommand :: Text -> Command
quitCommand callerName = Command $ do
    validatePlayer callerName callerName

    caller <- findPlayerBy_ name callerName

    tell [playerQuitMessage caller]

    removePlayer callerName
