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
playerQuitMessage caller = publicMessage [iFile|variant/standard/command/quit/player-quit.txt|]

playerVotedToBootMessage :: Player -> Player -> Message
playerVotedToBootMessage caller target = publicMessage [iFile|variant/standard/command/boot/player-voted-boot.txt|]

circleMessage :: Text -> [Player] -> Message
circleMessage to players = privateMessage to [iFile|variant/standard/command/circle/circle.txt|]

playerShotMessage :: Player -> Message
playerShotMessage player = publicMessage [iFile|variant/standard/command/choose/player-shot.txt|]

gameEndedMessage :: Text -> Message
gameEndedMessage callerName = publicMessage [iFile|variant/standard/command/end/game-ended.txt|]

gameDescriptionMessage :: Text -> Message
gameDescriptionMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/game-description.txt|]

globalCommandsMessage :: Text -> Message
globalCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/global-commands.txt|]

helpCommandsMessage :: Text -> Message
helpCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/help-commands.txt|]

hunterCommandsMessage :: Text -> Message
hunterCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/hunter-commands.txt|]

oracleCommandsMessage :: Text -> Message
oracleCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/oracle-commands.txt|]

orphanCommandsMessage :: Text -> Message
orphanCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/orphan-commands.txt|]

protectorCommandsMessage :: Text -> Message
protectorCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/protector-commands.txt|]

roleMessage :: Text -> Role -> Message
roleMessage callerName role = privateMessage callerName [iFile|variant/standard/command/help/role.txt|]

rulesMessage :: Text -> Message
rulesMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/rules.txt|]

scapegoatCommandsMessage :: Text -> Message
scapegoatCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/scapegoat-commands.txt|]

seerCommandsMessage :: Text -> Message
seerCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/seer-commands.txt|]

stagesMessage :: Text -> Maybe Game -> Message
stagesMessage callerName mGame = privateMessage callerName . T.concat $
    [ [iFile|variant/standard/command/help/standard-cycle.txt|]
    , [iFile|variant/standard/command/help/sunset.txt|]
    ] ++ [ [iFile|variant/standard/command/help/orphans-turn.txt|]
    | isNothing mGame || has (players . orphans . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/village-drunks-turn.txt|]
    | isNothing mGame || has (players . villageDrunks . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/seers-turn.txt|]
    | isNothing mGame || has (players . seers . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/oracles-turn.txt|]
    | isNothing mGame || has (players . oracles . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/protectors-turn.txt|]
    | isNothing mGame || has (players . protectors . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/werewolves-turn.txt|]
    ] ++ [ [iFile|variant/standard/command/help/witchs-turn.txt|]
    | isNothing mGame || has (players . witches . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/sunrise.txt|]
    ] ++ [ [iFile|variant/standard/command/help/hunters-turn.txt|]
    | isNothing mGame || has (players . hunters . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/druids-turn.txt|]
    | isNothing mGame || has (players . druids . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/villages-turn.txt|]
    ] ++ [ [iFile|variant/standard/command/help/hunters-turn.txt|]
    | isNothing mGame || has (players . hunters . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/scapegoats-turn.txt|]
    | isNothing mGame || has (players . scapegoats . named callerName) (fromJust mGame)
    ] ++ [ [iFile|variant/standard/command/help/game-over.txt|]
    ]

standardCommandsMessage :: Text -> Message
standardCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/standard-commands.txt|]

statusCommandsMessage :: Text -> Message
statusCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/status-commands.txt|]

witchCommandsMessage :: Text -> Message
witchCommandsMessage callerName = privateMessage callerName [iFile|variant/standard/command/help/witch-commands.txt|]

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to [iFile|variant/standard/command/ping/player-pinged.txt|]

pingRoleMessage :: Role -> Message
pingRoleMessage role = publicMessage [iFile|variant/standard/command/ping/role-pinged.txt|]

pingVillageMessage :: Message
pingVillageMessage = publicMessage [iFile|variant/standard/command/ping/village-pinged.txt|]

pingWerewolvesMessage :: Message
pingWerewolvesMessage = publicMessage [iFile|variant/standard/command/ping/werewolves-pinged.txt|]

currentStageMessages :: Text -> Game -> [Message]
currentStageMessages to game
    | has (stage . _GameOver) game  = [gameIsOverMessage to]
    | any (`is` (game ^. stage))
        [ _DruidsTurn
        , _Lynching
        , _Sunrise
        , _Sunset
        ]                           = []
    | otherwise                     = [privateMessage to [iFile|variant/standard/command/status/current-turn.txt|]]

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to [iFile|variant/standard/command/status/game-over.txt|]

playersInGameMessage :: Text -> Game -> Message
playersInGameMessage to game = privateMessage to . T.intercalate "\n" $
    alivePlayersText : [deadPlayersText | has (players . traverse . dead) game]
    where
        alivePlayersText    = [iFile|variant/standard/command/status/alive-players.txt|]
        deadPlayersText     = [iFile|variant/standard/command/status/dead-players.txt|]

playerRescindedVoteMessage :: Text -> Text -> Message
playerRescindedVoteMessage to caller = privateMessage to [iFile|variant/standard/command/unvote/player-rescinded-vote.txt|]

engineVersionMessage :: Text -> Message
engineVersionMessage to = privateMessage to [iFile|variant/standard/command/version/engine-version.txt|]

playerMadeDevourVoteMessage :: Text -> Player -> Player -> Message
playerMadeDevourVoteMessage to caller target = privateMessage to [iFile|variant/standard/command/vote/player-made-devour-vote.txt|]

playerMadeLynchVoteMessage :: Maybe Text -> Text -> Text -> Message
playerMadeLynchVoteMessage mTo caller target = Message mTo [iFile|variant/standard/command/vote/player-made-lynch-vote.txt|]
