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
playerQuitMessage caller = publicMessage [iFile|messages/command/quit/player-quit.text|]

playerVotedToBootMessage :: Player -> Player -> Message
playerVotedToBootMessage caller target = publicMessage [iFile|messages/command/boot/player-voted-boot.text|]

circleMessage :: Text -> [Player] -> Message
circleMessage to players = privateMessage to [iFile|messages/command/circle/circle.text|]

playerShotMessage :: Player -> Message
playerShotMessage player = publicMessage [iFile|messages/command/choose/player-shot.text|]

gameEndedMessage :: Text -> Message
gameEndedMessage callerName = publicMessage [iFile|messages/command/end/game-ended.text|]

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to [iFile|messages/command/ping/player-pinged.text|]

pingRoleMessage :: Role -> Message
pingRoleMessage role = publicMessage [iFile|messages/command/ping/role-pinged.text|]

pingVillageMessage :: Message
pingVillageMessage = publicMessage [iFile|messages/command/ping/village-pinged.text|]

pingWerewolvesMessage :: Message
pingWerewolvesMessage = publicMessage [iFile|messages/command/ping/werewolves-pinged.text|]

currentStageMessages :: Text -> Game -> [Message]
currentStageMessages to game
    | has (stage . _GameOver) game  = [gameIsOverMessage to]
    | any (`is` (game ^. stage))
        [ _DruidsTurn
        , _Lynching
        , _Sunrise
        , _Sunset
        ]                           = []
    | otherwise                     = [privateMessage to [iFile|messages/command/status/current-turn.text|]]

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to [iFile|messages/command/status/game-over.text|]

playersInGameMessage :: Text -> Game -> Message
playersInGameMessage to game = privateMessage to . T.intercalate "\n" $
    alivePlayersText : [deadPlayersText | has (players . traverse . dead) game]
    where
        alivePlayersText    = [iFile|messages/command/status/alive-players.text|]
        deadPlayersText     = [iFile|messages/command/status/dead-players.text|]

playerRescindedVoteMessage :: Text -> Text -> Message
playerRescindedVoteMessage to caller = privateMessage to [iFile|messages/command/unvote/player-rescinded-vote.text|]

engineVersionMessage :: Text -> Message
engineVersionMessage to = privateMessage to [iFile|messages/command/version/engine-version.text|]

playerMadeDevourVoteMessage :: Text -> Player -> Player -> Message
playerMadeDevourVoteMessage to caller target = privateMessage to [iFile|messages/command/vote/player-made-devour-vote.text|]

playerMadeLynchVoteMessage :: Maybe Text -> Text -> Text -> Message
playerMadeLynchVoteMessage mTo caller target = Message mTo [iFile|messages/command/vote/player-made-lynch-vote.text|]
