{-|
Module      : Game.Werewolf.Command.Orphan
Description : Orphan commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Orphan commands.
-}

module Game.Werewolf.Command.Orphan (
    -- * Commands
    chooseCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Message.Error
import Game.Werewolf.Util

chooseCommand :: Text -> Text -> Command
chooseCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerOrphan callerName) $ throwError [playerCannotDoThatMessage callerName]
    unlessM isOrphansTurn               $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (callerName == targetName)     $ throwError [playerCannotChooseSelfMessage callerName]
    validatePlayer callerName targetName

    roleModel .= Just targetName
