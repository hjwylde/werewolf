{-|
Module      : Game.Werewolf.Command.Scapegoat
Description : Scapegoat commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Scapegoat commands.
-}

module Game.Werewolf.Command.Scapegoat (
    -- * Commands
    chooseCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra

import Data.Text (Text)

import Game.Werewolf          hiding (doesPlayerExist)
import Game.Werewolf.Messages
import Game.Werewolf.Util

chooseCommand :: Text -> [Text] -> Command
chooseCommand callerName targetNames = Command $ do
    whenM isGameOver                        $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist callerName)    $ throwError [playerDoesNotExistMessage callerName callerName]
    unlessM (isPlayerScapegoat callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isScapegoatsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (null targetNames)                 $ throwError [playerMustChooseAtLeastOneTargetMessage callerName]
    when (callerName `elem` targetNames)    $ throwError [playerCannotChooseSelfMessage callerName]
    forM_ targetNames $ validatePlayer callerName
    whenM (use jesterRevealed &&^ anyM isPlayerJester targetNames) $
        throwError [playerCannotChooseJesterMessage callerName]

    allowedVoters   .= targetNames
    scapegoatBlamed .= False
