{-|
Module      : Game.Werewolf.Command.Oracle
Description : Oracle commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Seer commands.
-}

module Game.Werewolf.Command.Oracle (
    -- * Commands
    divineCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Message.Error
import Game.Werewolf.Util

divineCommand :: Text -> Text -> Command
divineCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerOracle callerName) $ throwError [playerCannotDoThatMessage callerName]
    unlessM isOraclesTurn               $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName

    divine .= Just targetName
