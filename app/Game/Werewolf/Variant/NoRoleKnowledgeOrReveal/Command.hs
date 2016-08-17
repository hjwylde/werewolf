{-|
Module      : Game.Werewolf.Variant.NoRoleKnowledgeOrReveal.Command
Description : Suite of command messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of command messages used throughout the werewolf game for the
'NoRoleKnowledgeOrReveal' variant.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Game.Werewolf.Variant.NoRoleKnowledgeOrReveal.Command (
    -- * Choose
    playerShotText,

    -- * Ping
    nocturnalRolePingedText, werewolvesPingedText,

    -- * Quit
    callerQuitText,

    -- * Status
    currentNocturnalTurnText, deadPlayersText,
) where

import Control.Lens

import Data.String.Humanise
import Data.String.Interpolate.Extra
import Data.Text                     (Text)

import Game.Werewolf
import Game.Werewolf.Message

playerShotText :: Player -> Text
playerShotText player = [iFile|variant/no-role-knowledge-or-reveal/command/choose/player-shot.txt|]

nocturnalRolePingedText :: Role -> Text
nocturnalRolePingedText _ = [iFile|variant/no-role-knowledge-or-reveal/command/ping/nocturnal-role-pinged.txt|]

werewolvesPingedText :: Text
werewolvesPingedText = [iFile|variant/no-role-knowledge-or-reveal/command/ping/werewolves-pinged.txt|]

callerQuitText :: Player -> Text
callerQuitText caller = [iFile|variant/no-role-knowledge-or-reveal/command/quit/caller-quit.txt|]

currentNocturnalTurnText :: Game -> Text
currentNocturnalTurnText _ = [iFile|variant/no-role-knowledge-or-reveal/command/status/current-nocturnal-turn.txt|]

deadPlayersText :: Game -> Text
deadPlayersText game = [iFile|variant/no-role-knowledge-or-reveal/command/status/dead-players.txt|]
