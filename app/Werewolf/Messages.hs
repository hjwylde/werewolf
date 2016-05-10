{-|
Module      : Werewolf.Messages
Description : Suite of messages used when calling the binary.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Suite of messages used when calling the binary.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Werewolf.Messages (
    -- * Command messages

    -- ** command.version
    engineVersionMessage,

    -- * Error messages

    -- ** error.command
    noGameRunningMessage,

    -- ** error.command.start
    gameAlreadyRunningMessage, roleDoesNotExistMessage,
) where

import Data.String.Interpolate.Extra
import Data.Text
import Data.Version

import Game.Werewolf

import Werewolf.Version

engineVersionMessage :: Text -> Message
engineVersionMessage to = privateMessage to [iFile|messages/command/version/engine-version.text|]

noGameRunningMessage :: Text -> Message
noGameRunningMessage to = privateMessage to [iFile|messages/error/command/no-game-running.text|]

gameAlreadyRunningMessage :: Text -> Message
gameAlreadyRunningMessage to = privateMessage to [iFile|messages/error/command/start/game-already-running.text|]

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to role = privateMessage to [iFile|messages/error/command/start/role-does-not-exist.text|]
