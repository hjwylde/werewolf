{-|
Module      : Game.Werewolf.Command.WildChild
Description : Wild-child commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Wild-child commands.
-}

module Game.Werewolf.Command.WildChild (
    -- * Commands
    chooseCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Messages
import Game.Werewolf.Util

chooseCommand :: Text -> Text -> Command
chooseCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWildChild callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWildChildsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (callerName == targetName)         $ throwError [playerCannotChooseSelfMessage callerName]
    validatePlayer callerName targetName

    roleModel .= Just targetName
