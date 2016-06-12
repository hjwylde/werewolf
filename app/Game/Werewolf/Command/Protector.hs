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

import Control.Lens.Extra
import Control.Monad.Except
import Control.Monad.Extra

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Message.Error
import Game.Werewolf.Util

protectCommand :: Text -> Text -> Command
protectCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerProtector callerName)                      $ throwError [playerCannotDoThatMessage callerName]
    unlessM isProtectorsTurn                                    $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName
    whenM (hasuse $ priorProtect . traverse . only targetName)  $ throwError [playerCannotProtectSamePlayerTwiceInARowMessage callerName]

    priorProtect    .= Just targetName
    protect         .= Just targetName
