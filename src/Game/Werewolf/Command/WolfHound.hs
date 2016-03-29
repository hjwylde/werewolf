{-|
Module      : Game.Werewolf.Command.WolfHound
Description : Wolf-hound commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Wolf-hound commands.
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Command.WolfHound (
    -- * Commands
    chooseCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra

import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

import Game.Werewolf
import Game.Werewolf.Messages
import Game.Werewolf.Util

chooseCommand :: Text -> Text -> Command
chooseCommand callerName allegianceName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWolfHound callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWolfHoundsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (isNothing mAllegiance)            $ throwError [allegianceDoesNotExistMessage callerName allegianceName]

    allegianceChosen .= mAllegiance
    where
        mAllegiance = case T.toLower allegianceName of
            "villagers"     -> Just Villagers
            "werewolves"    -> Just Werewolves
            _               -> Nothing
