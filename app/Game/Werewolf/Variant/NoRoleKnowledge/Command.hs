{-|
Module      : Game.Werewolf.Variant.NoRoleKnowledge.Command
Description : Suite of command messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of command messages used throughout the werewolf game for the 'NoRoleKnowledge'
variant.
-}

{-# LANGUAGE QuasiQuotes #-}

module Game.Werewolf.Variant.NoRoleKnowledge.Command (
    -- * Ping
    nocturnalRolePingedText, werewolvesPingedText,

    -- * Status
    currentNocturnalTurnText,
) where

import Data.String.Interpolate.Extra
import Data.Text                     (Text)

import Game.Werewolf

nocturnalRolePingedText :: Role -> Text
nocturnalRolePingedText _ = [iFile|variant/no-role-knowledge/command/ping/nocturnal-role-pinged.txt|]

werewolvesPingedText :: Text
werewolvesPingedText = [iFile|variant/no-role-knowledge/command/ping/werewolves-pinged.txt|]

currentNocturnalTurnText :: Game -> Text
currentNocturnalTurnText _ = [iFile|variant/no-role-knowledge/command/status/current-nocturnal-turn.txt|]
