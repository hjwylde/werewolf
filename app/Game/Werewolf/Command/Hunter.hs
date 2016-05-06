{-|
Module      : Game.Werewolf.Command.Hunter
Description : Hunter commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Hunter commands.
-}

module Game.Werewolf.Command.Hunter (
    -- * Commands
    chooseCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Writer

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Messages
import Game.Werewolf.Util

chooseCommand :: Text -> Text -> Command
chooseCommand callerName targetName = Command $ do
    whenM isGameOver                        $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist callerName)    $ throwError [playerDoesNotExistMessage callerName callerName]
    unlessM (isPlayerHunter callerName)     $ throwError [playerCannotDoThatMessage callerName]
    unlessM isHuntersTurn                   $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName

    target <- findPlayerBy_ name targetName

    tell [playerShotMessage target]
    killPlayer targetName

    hunterRetaliated .= True
