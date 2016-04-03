{-|
Module      : Werewolf.Messages
Description : Suite of messages used when calling the binary.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Suite of messages used when calling the binary.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Messages (
    -- * Binary messages
    engineVersionMessage,

    -- ** Error messages
    noGameRunningMessage, gameAlreadyRunningMessage, roleDoesNotExistMessage,
    playerCannotDoThatMessage, playerCannotDoThatRightNowMessage,
) where

import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Version

import Game.Werewolf

engineVersionMessage :: Text -> Version -> Message
engineVersionMessage to version =
    privateMessage to $ T.unwords ["Version", T.pack $ showVersion version]

noGameRunningMessage :: Text -> Message
noGameRunningMessage to = privateMessage to "No game is running."

gameAlreadyRunningMessage :: Text -> Message
gameAlreadyRunningMessage to = privateMessage to "A game is already running."

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to name = privateMessage to $ T.unwords ["Role", name, "does not exist."]

playerCannotDoThatMessage :: Text -> Message
playerCannotDoThatMessage to = privateMessage to "You cannot do that!"

playerCannotDoThatRightNowMessage :: Text -> Message
playerCannotDoThatRightNowMessage to = privateMessage to "You cannot do that right now!"
