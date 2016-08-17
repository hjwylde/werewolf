{-|
Module      : Game.Werewolf.Variant.NoRoleKnowledgeOrReveal.Engine
Description : Suite of engine messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of engine messages used throughout the werewolf game for the
'NoRoleKnowledgeOrReveal' variant.
-}

{-# LANGUAGE QuasiQuotes #-}

module Game.Werewolf.Variant.NoRoleKnowledgeOrReveal.Engine (
    -- * General
    playerBootedText, spitefulVillagerKilledText,

    -- * Lynching
    playerLynchedText, saintLynchedText, werewolfLynchedText,

    -- * New game
    rolesInGameText,

    -- * Sunrise
    playerDevouredText, playerPoisonedText, playerTurnedToStoneText,
) where

import Control.Lens

import Data.String.Humanise
import Data.String.Interpolate.Extra
import Data.Text                     as T

import Game.Werewolf
import Game.Werewolf.Message

playerBootedText :: Player -> Text
playerBootedText player = [iFile|variant/no-role-knowledge-or-reveal/engine/general/player-booted.txt|]

spitefulVillagerKilledText :: Game -> Text
spitefulVillagerKilledText game = [iFile|variant/no-role-knowledge-or-reveal/engine/general/spiteful-villager-killed.txt|]

playerLynchedText :: Player -> Text
playerLynchedText player = [iFile|variant/no-role-knowledge-or-reveal/engine/lynching/player-lynched.txt|]

saintLynchedText :: [Player] -> Text
saintLynchedText voters = [iFile|variant/no-role-knowledge-or-reveal/engine/lynching/saint-lynched.txt|]

werewolfLynchedText :: Player -> Text
werewolfLynchedText werewolf = [iFile|variant/no-role-knowledge-or-reveal/engine/lynching/werewolf-lynched.txt|]

rolesInGameText :: Game -> Text
rolesInGameText game = [iFile|variant/no-role-knowledge-or-reveal/engine/new-game/roles-in-game.txt|]
    where
        totalBalance = sumOf (players . roles . balance) game

playerDevouredText :: Player -> Text
playerDevouredText player = [iFile|variant/no-role-knowledge-or-reveal/engine/sunrise/player-devoured.txt|]

playerPoisonedText :: Player -> Text
playerPoisonedText player = [iFile|variant/no-role-knowledge-or-reveal/engine/sunrise/player-poisoned.txt|]

playerTurnedToStoneText :: Player -> Text
playerTurnedToStoneText player = [iFile|variant/no-role-knowledge-or-reveal/engine/sunrise/player-turned-to-stone.txt|]
