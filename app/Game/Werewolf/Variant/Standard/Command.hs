{-|
Module      : Game.Werewolf.Variant.Standard.Command
Description : Suite of command messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of command messages used throughout the werewolf game for the 'Standard' variant.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Game.Werewolf.Variant.Standard.Command (
    -- * Boot
    callerVotedBootText,

    -- * Circle
    gameCircleText,

    -- * Choose
    playerShotText,

    -- * End
    gameEndedText,

    -- * Help
    druidsTurnText, gameDescriptionText, gameRulesText, globalCommandsText, helpCommandsText,
    hunterCommandsText, huntersTurnText, necromancerCommandsText, necromancersTurnText,
    oracleCommandsText, oraclesTurnText, orphanCommandsText, orphansTurnText, protectorCommandsText,
    protectorsTurnText, roleDescriptionText, scapegoatCommandsText, scapegoatsTurnText,
    seerCommandsText, seersTurnText, standardCommandsText, standardCycleText, statusCommandsText,
    sunriseText, sunsetText, villageDrunksTurnText, villagesTurnText, werewolvesTurnText,
    winConditionText, witchCommandsText, witchsTurnText,

    -- * Ping
    diurnalRolePingedText, nocturnalRolePingedText, playerPingedText, villagePingedText,
    werewolvesPingedText,

    -- * Quit
    callerQuitText,

    -- * Raise
    necromancerRaisedDeadText, playerRaisedFromDeadText,

    -- * Status
    alivePlayersText, currentDiurnalTurnText, currentNocturnalTurnText, deadPlayersText,
    gameOverText, marksText,

    -- * Unvote
    callerRescindedVoteText,

    -- * Version
    engineVersionText,

    -- * Vote
    callerVotedDevourText, callerVotedLynchText,
) where

import Control.Lens

import           Data.String.Humanise
import           Data.String.Interpolate.Extra
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Version

import Game.Werewolf
import Game.Werewolf.Message

import Werewolf.Version

callerVotedBootText :: Player -> Player -> Text
callerVotedBootText caller target = [iFile|variant/standard/command/boot/player-voted-boot.txt|]

gameCircleText :: [Player] -> Text
gameCircleText players = [iFile|variant/standard/command/circle/game-circle.txt|]

playerShotText :: Player -> Text
playerShotText player = [iFile|variant/standard/command/choose/player-shot.txt|]

gameEndedText :: Text -> Text
gameEndedText callerName = [iFile|variant/standard/command/end/game-ended.txt|]

druidsTurnText :: Text
druidsTurnText = [iFile|variant/standard/command/help/druids-turn.txt|]

gameDescriptionText :: Text
gameDescriptionText = [iFile|variant/standard/command/help/game-description.txt|]

gameRulesText :: Text
gameRulesText = [iFile|variant/standard/command/help/rules.txt|]

globalCommandsText :: Text
globalCommandsText = [iFile|variant/standard/command/help/global-commands.txt|]

helpCommandsText :: Text
helpCommandsText = [iFile|variant/standard/command/help/help-commands.txt|]

hunterCommandsText :: Text
hunterCommandsText = [iFile|variant/standard/command/help/hunter-commands.txt|]

huntersTurnText :: Text
huntersTurnText = [iFile|variant/standard/command/help/hunters-turn.txt|]

necromancerCommandsText :: Text
necromancerCommandsText = [iFile|variant/standard/command/help/necromancer-commands.txt|]

necromancersTurnText :: Text
necromancersTurnText = [iFile|variant/standard/command/help/necromancers-turn.txt|]

oracleCommandsText :: Text
oracleCommandsText = [iFile|variant/standard/command/help/oracle-commands.txt|]

oraclesTurnText :: Text
oraclesTurnText = [iFile|variant/standard/command/help/oracles-turn.txt|]

orphanCommandsText :: Text
orphanCommandsText = [iFile|variant/standard/command/help/orphan-commands.txt|]

orphansTurnText :: Text
orphansTurnText = [iFile|variant/standard/command/help/orphans-turn.txt|]

protectorCommandsText :: Text
protectorCommandsText = [iFile|variant/standard/command/help/protector-commands.txt|]

protectorsTurnText :: Text
protectorsTurnText = [iFile|variant/standard/command/help/protectors-turn.txt|]

roleDescriptionText :: Role -> Text
roleDescriptionText role = [iFile|variant/standard/command/help/role.txt|]

scapegoatCommandsText :: Text
scapegoatCommandsText = [iFile|variant/standard/command/help/scapegoat-commands.txt|]

scapegoatsTurnText :: Text
scapegoatsTurnText = [iFile|variant/standard/command/help/scapegoats-turn.txt|]

seerCommandsText :: Text
seerCommandsText = [iFile|variant/standard/command/help/seer-commands.txt|]

seersTurnText :: Text
seersTurnText = [iFile|variant/standard/command/help/seers-turn.txt|]

standardCommandsText :: Text
standardCommandsText = [iFile|variant/standard/command/help/standard-commands.txt|]

standardCycleText :: Text
standardCycleText = [iFile|variant/standard/command/help/standard-cycle.txt|]

statusCommandsText :: Text
statusCommandsText = [iFile|variant/standard/command/help/status-commands.txt|]

sunriseText :: Text
sunriseText = [iFile|variant/standard/command/help/sunrise.txt|]

sunsetText :: Text
sunsetText = [iFile|variant/standard/command/help/sunset.txt|]

villageDrunksTurnText :: Text
villageDrunksTurnText = [iFile|variant/standard/command/help/village-drunks-turn.txt|]

villagesTurnText :: Text
villagesTurnText = [iFile|variant/standard/command/help/villages-turn.txt|]

werewolvesTurnText :: Text
werewolvesTurnText = [iFile|variant/standard/command/help/werewolves-turn.txt|]

winConditionText :: Text
winConditionText = [iFile|variant/standard/command/help/win-condition.txt|]

witchCommandsText :: Text
witchCommandsText = [iFile|variant/standard/command/help/witch-commands.txt|]

witchsTurnText :: Text
witchsTurnText = [iFile|variant/standard/command/help/witchs-turn.txt|]

diurnalRolePingedText :: Role -> Text
diurnalRolePingedText role = [iFile|variant/standard/command/ping/diurnal-role-pinged.txt|]

nocturnalRolePingedText :: Role -> Text
nocturnalRolePingedText role = [iFile|variant/standard/command/ping/nocturnal-role-pinged.txt|]

playerPingedText :: Text
playerPingedText = [iFile|variant/standard/command/ping/player-pinged.txt|]

villagePingedText :: Text
villagePingedText = [iFile|variant/standard/command/ping/village-pinged.txt|]

werewolvesPingedText :: Text
werewolvesPingedText = [iFile|variant/standard/command/ping/werewolves-pinged.txt|]

callerQuitText :: Player -> Text
callerQuitText caller = [iFile|variant/standard/command/quit/caller-quit.txt|]

necromancerRaisedDeadText :: Game -> Text
necromancerRaisedDeadText game = [iFile|variant/standard/command/raise/necromancer-raised-dead.txt|]
    where
        zombies' = game ^.. players . zombies

playerRaisedFromDeadText :: Game -> Text
playerRaisedFromDeadText game = [iFile|variant/standard/command/raise/player-raised-from-dead.txt|]
    where
        necromancer' = game ^?! players . necromancers

marksText :: Game -> Text
marksText game = [iFile|variant/standard/command/status/marks.txt|]
    where
        marks' = game ^.. players . traverse . filtered (\player -> player ^. name `elem` game ^. marks)

alivePlayersText :: Game -> Text
alivePlayersText game = [iFile|variant/standard/command/status/alive-players.txt|]

currentDiurnalTurnText :: Game -> Text
currentDiurnalTurnText game = [iFile|variant/standard/command/status/current-diurnal-turn.txt|]

currentNocturnalTurnText :: Game -> Text
currentNocturnalTurnText game = [iFile|variant/standard/command/status/current-nocturnal-turn.txt|]

deadPlayersText :: Game -> Text
deadPlayersText game = [iFile|variant/standard/command/status/dead-players.txt|]

gameOverText :: Text
gameOverText = [iFile|variant/standard/command/status/game-over.txt|]

callerRescindedVoteText :: Player -> Text
callerRescindedVoteText caller = [iFile|variant/standard/command/unvote/player-rescinded-vote.txt|]

engineVersionText :: Text
engineVersionText = [iFile|variant/standard/command/version/engine-version.txt|]

callerVotedDevourText :: Player -> Player -> Text
callerVotedDevourText caller target = [iFile|variant/standard/command/vote/player-made-devour-vote.txt|]

callerVotedLynchText :: Player -> Player -> Text
callerVotedLynchText caller target = [iFile|variant/standard/command/vote/player-made-lynch-vote.txt|]
