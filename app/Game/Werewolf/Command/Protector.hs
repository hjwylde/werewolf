{-|
Module      : Game.Werewolf.Command.Protector
Description : Protector commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Protector commands.
-}

module Game.Werewolf.Command.Protector (
    -- * Commands
    protectCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Messages
import Game.Werewolf.Util

protectCommand :: Text -> Text -> Command
protectCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerProtector callerName)                          $ throwError [playerCannotDoThatMessage callerName]
    unlessM isProtectorsTurn                                        $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName
    whenM (has (priorProtect . traverse . only targetName) <$> get) $ throwError [playerCannotProtectSamePlayerTwiceInARowMessage callerName]

    priorProtect    .= Just targetName
    protect         .= Just targetName
