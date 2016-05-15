{-|
Module      : Game.Werewolf.Message.Command
Description : Suite of command messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of command messages used throughout the werewolf game.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Game.Werewolf.Message.Command (
    -- * Quit
    playerQuitMessage,

    -- * Boot
    playerVotedToBootMessage,

    -- * Circle
    circleMessage,

    -- * Choose
    playerShotMessage,

    -- * End
    gameEndedMessage,

    -- * Help
    gameDescriptionMessage, globalCommandsMessage, helpCommandsMessage, hunterCommandsMessage,
    oracleCommandsMessage, orphanCommandsMessage, protectorCommandsMessage, roleMessage,
    rulesMessage, scapegoatCommandsMessage, seerCommandsMessage, stagesMessage,
    standardCommandsMessage, statusCommandsMessage, witchCommandsMessage,

    -- * Ping
    pingPlayerMessage, pingRoleMessage, pingVillageMessage, pingWerewolvesMessage,

    -- * Status
    currentStageMessages, gameIsOverMessage, playersInGameMessage,

    -- * Unvote
    playerRescindedVoteMessage,

    -- * Version
    engineVersionMessage,

    -- * Vote
    playerMadeDevourVoteMessage, playerMadeLynchVoteMessage,
) where

import Control.Lens       hiding (isn't)
import Control.Lens.Extra

import           Data.Maybe
import           Data.String.Humanise
import           Data.String.Interpolate.Extra
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Version

import Game.Werewolf.Game
import Game.Werewolf.Message
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role     hiding (name)

import Werewolf.Version

playerQuitMessage :: Player -> Message
playerQuitMessage caller = publicMessage [iFile|variant/standard/command/quit/player-quit.text|]

playerVotedToBootMessage :: Player -> Player -> Message
playerVotedToBootMessage caller target = publicMessage [iFile|variant/standard/command/boot/player-voted-boot.text|]

circleMessage :: Text -> [Player] -> Message
circleMessage to players = privateMessage to [iFile|variant/standard/command/circle/circle.text|]

playerShotMessage :: Player -> Message
playerShotMessage player = publicMessage [iFile|variant/standard/command/choose/player-shot.text|]

gameEndedMessage :: Text -> Message
gameEndedMessage callerName = publicMessage [iFile|variant/standard/command/end/game-ended.text|]

gameDescriptionMessage :: Text -> Message
gameDescriptionMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/game-description.text|]

globalCommandsMessage :: Text -> Message
globalCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/global-commands.text|]

helpCommandsMessage :: Text -> Message
helpCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/help-commands.text|]

hunterCommandsMessage :: Text -> Message
hunterCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/hunter-commands.text|]

oracleCommandsMessage :: Text -> Message
oracleCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/oracle-commands.text|]

orphanCommandsMessage :: Text -> Message
orphanCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/orphan-commands.text|]

protectorCommandsMessage :: Text -> Message
protectorCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/protector-commands.text|]

roleMessage :: Text -> Role -> Message
roleMessage callerName role = privateMessage callerName [iFile|variant/standard/command/help/role.text|]

rulesMessage :: Text -> Message
rulesMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/rules.text|]

scapegoatCommandsMessage :: Text -> Message
scapegoatCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/scapegoat-commands.text|]

seerCommandsMessage :: Text -> Message
seerCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/seer-commands.text|]

stagesMessage :: Text -> Maybe Game -> Message
stagesMessage callerName mGame = privateMessage callerName . T.concat $
    [ [iFile|variant/standard/command/help/standard-cycle.text|]
    , [iFile|variant/standard/command/help/sunset.text|]
    ] ++ [ [iFile|variant/standard/command/help/orphans-turn.text|]
    | isNothing mGame || has (players . orphans . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/village-drunks-turn.text|]
    | isNothing mGame || has (players . villageDrunks . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/seers-turn.text|]
    | isNothing mGame || has (players . seers . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/oracles-turn.text|]
    | isNothing mGame || has (players . oracles . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/protectors-turn.text|]
    | isNothing mGame || has (players . protectors . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/werewolves-turn.text|]
    ] ++ [ [iFile|variant/standard/command/help/witchs-turn.text|]
    | isNothing mGame || has (players . witches . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/sunrise.text|]
    ] ++ [ [iFile|variant/standard/command/help/hunters-turn.text|]
    | isNothing mGame || has (players . hunters . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/druids-turn.text|]
    | isNothing mGame || has (players . druids . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/villages-turn.text|]
    ] ++ [ [iFile|variant/standard/command/help/hunters-turn.text|]
    | isNothing mGame || has (players . hunters . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/scapegoats-turn.text|]
    | isNothing mGame || has (players . scapegoats . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/game-over.text|]
    ]

standardCommandsMessage :: Text -> Message
standardCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/standard-commands.text|]

statusCommandsMessage :: Text -> Message
statusCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/status-commands.text|]

witchCommandsMessage :: Text -> Message
witchCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/witch-commands.text|]

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to [iFile|variant/standard/command/ping/player-pinged.text|]

pingRoleMessage :: Role -> Message
pingRoleMessage role = publicMessage [iFile|variant/standard/command/ping/role-pinged.text|]

pingVillageMessage :: Message
pingVillageMessage = publicMessage [iFile|variant/standard/command/ping/village-pinged.text|]

pingWerewolvesMessage :: Message
pingWerewolvesMessage = publicMessage [iFile|variant/standard/command/ping/werewolves-pinged.text|]

currentStageMessages :: Text -> Game -> [Message]
currentStageMessages to game
    | has (stage . _GameOver) game  = [gameIsOverMessage to]
    | any (`is` (game ^. stage))
        [ _DruidsTurn
        , _Lynching
        , _Sunrise
        , _Sunset
        ]                           = []
    | otherwise                     = [privateMessage to [iFile|variant/standard/command/status/current-turn.text|]]

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to [iFile|variant/standard/command/status/game-over.text|]

playersInGameMessage :: Text -> Game -> Message
playersInGameMessage to game = privateMessage to . T.intercalate "\n" $
    alivePlayersText : [deadPlayersText | has (players . traverse . dead) game]
    where
        alivePlayersText    = [iFile|variant/standard/command/status/alive-players.text|]
        deadPlayersText     = [iFile|variant/standard/command/status/dead-players.text|]

playerRescindedVoteMessage :: Text -> Text -> Message
playerRescindedVoteMessage to caller = privateMessage to [iFile|variant/standard/command/unvote/player-rescinded-vote.text|]

engineVersionMessage :: Text -> Message
engineVersionMessage to = privateMessage to [iFile|variant/standard/command/version/engine-version.text|]

playerMadeDevourVoteMessage :: Text -> Player -> Player -> Message
playerMadeDevourVoteMessage to caller target = privateMessage to [iFile|variant/standard/command/vote/player-made-devour-vote.text|]

playerMadeLynchVoteMessage :: Maybe Text -> Text -> Text -> Message
playerMadeLynchVoteMessage mTo caller target = Message mTo [iFile|variant/standard/command/vote/player-made-lynch-vote.text|]
