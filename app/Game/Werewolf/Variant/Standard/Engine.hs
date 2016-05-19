{-|
Module      : Game.Werewolf.Variant.Standard.Engine
Description : Suite of engine messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of engine messages used throughout the werewolf game for the 'Standard' variant.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Game.Werewolf.Variant.Standard.Engine (
    -- * Druid's turn
    druidsTurnText,

    -- * General
    playerBootedText, playerKilledText,

    -- * Game over
    allegianceWonText, fallenAngelWonText, playerContributedText, playerLostText, playerRolesText,
    playerWonText,

    -- * Hunter's turn
    huntersTurnPrivateText, huntersTurnPublicText,

    -- * Lynching
    jesterLynchedText, noPlayerLynchedText, playerLynchedText, scapegoatLynchedText,
    werewolfLynchedText,

    -- * New game
    beholderText, fallenAngelText, newPlayerText, playersInGameText, rolesInGameText,
    spitefulGhostText, trueVillagerText,

    -- * Oracle's turn
    oraclesTurnPrivateText, oraclesTurnPublicText,

    -- * Orphan's turn
    orphanJoinedWerewolvesGroupText, orphanJoinedWerewolvesPrivateText, orphansTurnPrivateText,
    orphansTurnPublicText,

    -- * Protector's turn
    protectorsTurnPrivateText, protectorsTurnPublicText,

    -- * Scapegoat's turn
    scapegoatsTurnText, scapegoatsTurnEndedText,

    -- * Seer's turn
    seersTurnPrivateText, seersTurnPublicText, alphaWolfSeenText, lycanSeenText,

    -- * Sunrise
    noPlayerDevouredText, playerDevouredText, playerDivinedText, playerPoisonedText, playerSeenText,
    sunriseText, playerTurnedToStoneText,

    -- * Sunset
    sunsetText,

    -- * Village Drunk's turn
    villageDrunkJoinedVillageText, villageDrunkJoinedWerewolvesGroupText,
    villageDrunkJoinedWerewolvesPrivateText, villageDrunksTurnText,

    -- * Village's turn
    villagesTurnText,

    -- * Werewolves' turn
    firstWerewolvesTurnText, werewolvesTurnPrivateText, werewolvesTurnPublicText,

    -- * Witch's turn
    healText, passText, poisonText, witchsTurnText,
) where

import Control.Arrow
import Control.Lens       hiding (isn't)
import Control.Lens.Extra

import           Data.List.Extra
import           Data.String.Humanise
import           Data.String.Interpolate.Extra
import           Data.Text                     (Text)
import qualified Data.Text                     as T

import Game.Werewolf
import Game.Werewolf.Message

druidsTurnText :: Text
druidsTurnText = [iFile|variant/standard/engine/druids-turn/start.txt|]

playerBootedText :: Player -> Text
playerBootedText player = [iFile|variant/standard/engine/general/player-booted.txt|]

playerKilledText :: Text
playerKilledText = [iFile|variant/standard/engine/general/player-killed.txt|]

allegianceWonText :: Allegiance -> Text
allegianceWonText allegiance = [iFile|variant/standard/engine/game-over/allegiance-won.txt|]

fallenAngelWonText :: Text
fallenAngelWonText = [iFile|variant/standard/engine/game-over/fallen-angel-won.txt|]

playerContributedText :: Text
playerContributedText = [iFile|variant/standard/engine/game-over/player-contributed.txt|]

playerLostText :: Text
playerLostText = [iFile|variant/standard/engine/game-over/player-lost.txt|]

playerRolesText :: Game -> Text
playerRolesText game = [iFile|variant/standard/engine/game-over/player-roles.txt|]

playerWonText :: Text
playerWonText = [iFile|variant/standard/engine/game-over/player-won.txt|]

huntersTurnPrivateText :: Text
huntersTurnPrivateText = [iFile|variant/standard/engine/hunters-turn/start-private.txt|]

huntersTurnPublicText :: Game -> Text
huntersTurnPublicText game = [iFile|variant/standard/engine/hunters-turn/start-public.txt|]
    where
        hunter = game ^?! players . hunters

jesterLynchedText :: Game -> Text
jesterLynchedText game = [iFile|variant/standard/engine/lynching/jester-lynched.txt|]
    where
        jester = game ^?! players . jesters

noPlayerLynchedText :: Text
noPlayerLynchedText = [iFile|variant/standard/engine/lynching/no-player-lynched.txt|]

playerLynchedText :: Player -> Text
playerLynchedText player = [iFile|variant/standard/engine/lynching/player-lynched.txt|]

scapegoatLynchedText :: Game -> Text
scapegoatLynchedText game = [iFile|variant/standard/engine/lynching/scapegoat-lynched.txt|]
    where
        scapegoat = game ^?! players . scapegoats

werewolfLynchedText :: Player -> Text
werewolfLynchedText werewolf = [iFile|variant/standard/engine/lynching/werewolf-lynched.txt|]

beholderText :: Game -> Text
beholderText game = [iFile|variant/standard/engine/new-game/beholder.txt|]
    where
        seer = game ^?! players . seers

fallenAngelText :: Text
fallenAngelText = [iFile|variant/standard/engine/new-game/fallen-angel.txt|]

newPlayerText :: Player -> Text
newPlayerText player = [iFile|variant/standard/engine/new-game/new-player.txt|]

playersInGameText :: Game -> Text
playersInGameText game = [iFile|variant/standard/engine/new-game/players-in-game.txt|]

rolesInGameText :: Game -> Text
rolesInGameText game = [iFile|variant/standard/engine/new-game/roles-in-game.txt|]
    where
        roles           = game ^.. players . traverse . role
        roleCounts      = map (head &&& length) (groupSortOn humanise roles)
        totalBalance    = sumOf (traverse . balance) roles

spitefulGhostText :: Game -> Text
spitefulGhostText game = [iFile|variant/standard/engine/new-game/spiteful-ghost.txt|]

trueVillagerText :: Game -> Text
trueVillagerText game = [iFile|variant/standard/engine/new-game/true-villager.txt|]
    where
        trueVillager = game ^?! players . trueVillagers

oraclesTurnPrivateText :: Text
oraclesTurnPrivateText = [iFile|variant/standard/engine/oracles-turn/start-private.txt|]

oraclesTurnPublicText :: Text
oraclesTurnPublicText = [iFile|variant/standard/engine/oracles-turn/start-public.txt|]

orphanJoinedWerewolvesGroupText :: Game -> Text
orphanJoinedWerewolvesGroupText game = [iFile|variant/standard/engine/orphans-turn/player-joined-werewolves-group.txt|]
    where
        orphan = game ^?! players . orphans

orphanJoinedWerewolvesPrivateText :: Game -> Text
orphanJoinedWerewolvesPrivateText game = [iFile|variant/standard/engine/orphans-turn/player-joined-werewolves-private.txt|]
    where
        orphan      = game ^?! players . orphans
        werewolves  = game ^.. players . traverse . alive . filtered (is werewolf) \\ [orphan]

orphansTurnPrivateText :: Text
orphansTurnPrivateText = [iFile|variant/standard/engine/orphans-turn/start-private.txt|]

orphansTurnPublicText :: Text
orphansTurnPublicText = [iFile|variant/standard/engine/orphans-turn/start-public.txt|]

protectorsTurnPrivateText :: Text
protectorsTurnPrivateText = [iFile|variant/standard/engine/protectors-turn/start-private.txt|]

protectorsTurnPublicText :: Text
protectorsTurnPublicText = [iFile|variant/standard/engine/protectors-turn/start-public.txt|]

scapegoatsTurnText :: Game -> Text
scapegoatsTurnText game = [iFile|variant/standard/engine/scapegoats-turn/start.txt|]
    where
        scapegoat = game ^?! players . scapegoats

scapegoatsTurnEndedText :: Game -> Text
scapegoatsTurnEndedText game = [iFile|variant/standard/engine/scapegoats-turn/end.txt|]

seersTurnPrivateText :: Text
seersTurnPrivateText = [iFile|variant/standard/engine/seers-turn/start-private.txt|]

seersTurnPublicText :: Text
seersTurnPublicText = [iFile|variant/standard/engine/seers-turn/start-public.txt|]

alphaWolfSeenText :: Player -> Text
alphaWolfSeenText alphaWolf = [iFile|variant/standard/engine/sunrise/alpha-wolf-seen.txt|]

lycanSeenText :: Player -> Text
lycanSeenText lycan = [iFile|variant/standard/engine/sunrise/lycan-seen.txt|]

noPlayerDevouredText :: Text
noPlayerDevouredText = [iFile|variant/standard/engine/sunrise/no-player-devoured.txt|]

playerDevouredText :: Player -> Text
playerDevouredText player = [iFile|variant/standard/engine/sunrise/player-devoured.txt|]

playerDivinedText :: Player -> Text
playerDivinedText player = [iFile|variant/standard/engine/sunrise/player-divined.txt|]

playerPoisonedText :: Player -> Text
playerPoisonedText player = [iFile|variant/standard/engine/sunrise/player-poisoned.txt|]

playerSeenText :: Player -> Text
playerSeenText player = [iFile|variant/standard/engine/sunrise/player-seen.txt|]
    where
        article = if is loner player then "" else "the " :: Text

sunriseText :: Text
sunriseText = [iFile|variant/standard/engine/sunrise/start.txt|]

playerTurnedToStoneText :: Player -> Text
playerTurnedToStoneText player = [iFile|variant/standard/engine/sunrise/player-turned-to-stone.txt|]

sunsetText :: Text
sunsetText = [iFile|variant/standard/engine/sunset/start.txt|]

villageDrunkJoinedVillageText :: Text
villageDrunkJoinedVillageText = [iFile|variant/standard/engine/village-drunks-turn/player-joined-village.txt|]

villageDrunkJoinedWerewolvesGroupText :: Game -> Text
villageDrunkJoinedWerewolvesGroupText game = [iFile|variant/standard/engine/village-drunks-turn/player-joined-werewolves-group.txt|]
    where
        villageDrunk = game ^?! players . villageDrunks

villageDrunkJoinedWerewolvesPrivateText :: Game -> Text
villageDrunkJoinedWerewolvesPrivateText game = [iFile|variant/standard/engine/village-drunks-turn/player-joined-werewolves-private.txt|]
    where
        villageDrunk    = game ^?! players . villageDrunks
        werewolves      = game ^.. players . traverse . alive . filtered (is werewolf) \\ [villageDrunk]

villageDrunksTurnText :: Text
villageDrunksTurnText = [iFile|variant/standard/engine/village-drunks-turn/start.txt|]

villagesTurnText :: Text
villagesTurnText = [iFile|variant/standard/engine/villages-turn/start.txt|]

firstWerewolvesTurnText :: Player -> Game -> Text
firstWerewolvesTurnText player game = [iFile|variant/standard/engine/werewolves-turn/start-first-round-private.txt|]
    where
        werewolves = game ^.. players . traverse . alive . filtered (is werewolf)

werewolvesTurnPrivateText :: Text
werewolvesTurnPrivateText = [iFile|variant/standard/engine/werewolves-turn/start-private.txt|]

werewolvesTurnPublicText :: Text
werewolvesTurnPublicText = [iFile|variant/standard/engine/werewolves-turn/start-public.txt|]

healText :: Game -> Text
healText game = [iFile|variant/standard/engine/witchs-turn/heal.txt|]
    where
        victim = game ^?! votee

passText :: Text
passText = [iFile|variant/standard/engine/witchs-turn/pass.txt|]

poisonText :: Text
poisonText = [iFile|variant/standard/engine/witchs-turn/poison.txt|]

witchsTurnText :: Text
witchsTurnText = [iFile|variant/standard/engine/witchs-turn/start.txt|]
