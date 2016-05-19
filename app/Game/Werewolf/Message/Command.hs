{-|
Module      : Game.Werewolf.Message.Command
Description : Suite of command messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of command messages used throughout the werewolf game.
-}

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
    currentStageMessage, gameIsOverMessage, playersInGameMessage,

    -- * Unvote
    playerRescindedVoteMessage,

    -- * Version
    engineVersionMessage,

    -- * Vote
    playerMadeDevourVoteMessage, playerMadeLynchVoteMessage,
) where

import Control.Lens

import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role
import Game.Werewolf.Variant.NoRoleKnowledge.Command as NoRoleKnowledge
import Game.Werewolf.Variant.Standard.Command        as Standard

playerQuitMessage :: Player -> Message
playerQuitMessage = publicMessage . callerQuitText

playerVotedToBootMessage :: Player -> Player -> Message
playerVotedToBootMessage caller = publicMessage . callerVotedBootText caller

circleMessage :: Text -> [Player] -> Message
circleMessage to = privateMessage to . gameCircleText

playerShotMessage :: Player -> Message
playerShotMessage = publicMessage . playerShotText

gameEndedMessage :: Text -> Message
gameEndedMessage = publicMessage . gameEndedText

gameDescriptionMessage :: Text -> Message
gameDescriptionMessage to = privateMessage to gameDescriptionText

globalCommandsMessage :: Text -> Message
globalCommandsMessage to = privateMessage to globalCommandsText

helpCommandsMessage :: Text -> Message
helpCommandsMessage to = privateMessage to helpCommandsText

hunterCommandsMessage :: Text -> Message
hunterCommandsMessage to = privateMessage to hunterCommandsText

oracleCommandsMessage :: Text -> Message
oracleCommandsMessage to = privateMessage to oracleCommandsText

orphanCommandsMessage :: Text -> Message
orphanCommandsMessage to = privateMessage to orphanCommandsText

protectorCommandsMessage :: Text -> Message
protectorCommandsMessage to = privateMessage to protectorCommandsText

roleMessage :: Text -> Role -> Message
roleMessage to = privateMessage to . roleDescriptionText

rulesMessage :: Text -> Message
rulesMessage to = privateMessage to gameRulesText

scapegoatCommandsMessage :: Text -> Message
scapegoatCommandsMessage to = privateMessage to scapegoatCommandsText

seerCommandsMessage :: Text -> Message
seerCommandsMessage to = privateMessage to seerCommandsText

stagesMessage :: Text -> Maybe Game -> Message
stagesMessage to mGame = privateMessage to . T.concat $
    [ standardCycleText
    , sunsetText
    ] ++ [ orphansTurnText
    | isNothing mGame || has (players . orphans . named to) (fromJust mGame)
    ] ++ [ villageDrunksTurnText
    | isNothing mGame || has (players . villageDrunks . named to) (fromJust mGame)
    ] ++ [ seersTurnText
    | isNothing mGame || has (players . seers . named to) (fromJust mGame)
    ] ++ [ oraclesTurnText
    | isNothing mGame || has (players . oracles . named to) (fromJust mGame)
    ] ++ [ protectorsTurnText
    | isNothing mGame || has (players . protectors . named to) (fromJust mGame)
    ] ++ [ werewolvesTurnText
    ] ++ [ witchsTurnText
    | isNothing mGame || has (players . witches . named to) (fromJust mGame)
    ] ++ [ sunriseText
    ] ++ [ huntersTurnText
    | isNothing mGame || has (players . hunters . named to) (fromJust mGame)
    ] ++ [ druidsTurnText
    | isNothing mGame || has (players . druids . named to) (fromJust mGame)
    ] ++ [ villagesTurnText
    ] ++ [ huntersTurnText
    | isNothing mGame || has (players . hunters . named to) (fromJust mGame)
    ] ++ [ scapegoatsTurnText
    | isNothing mGame || has (players . scapegoats . named to) (fromJust mGame)
    ] ++ [ winConditionText
    ]

standardCommandsMessage :: Text -> Message
standardCommandsMessage to = privateMessage to standardCommandsText

statusCommandsMessage :: Text -> Message
statusCommandsMessage to = privateMessage to statusCommandsText

witchCommandsMessage :: Text -> Message
witchCommandsMessage to = privateMessage to witchCommandsText

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to playerPingedText

pingRoleMessage :: Role -> Game -> Message
pingRoleMessage role game
    | has (variant . _NoRoleKnowledge) game = publicMessage $ NoRoleKnowledge.rolePingedText role
    | otherwise                             = publicMessage $ Standard.rolePingedText role

pingVillageMessage :: Message
pingVillageMessage = publicMessage villagePingedText

pingWerewolvesMessage :: Game -> Message
pingWerewolvesMessage game
    | has (variant . _NoRoleKnowledge) game = publicMessage NoRoleKnowledge.werewolvesPingedText
    | otherwise                             = publicMessage Standard.werewolvesPingedText

currentStageMessage :: Text -> Game -> Message
currentStageMessage to game
    | has (stage . _GameOver) game          = gameIsOverMessage to
    | has (variant . _NoRoleKnowledge) game = privateMessage to $ NoRoleKnowledge.currentTurnText game
    | otherwise                             = privateMessage to $ Standard.currentTurnText game

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to gameOverText

playersInGameMessage :: Text -> Game -> Message
playersInGameMessage to game = privateMessage to $ T.append alivePlayersText' deadPlayersText'
    where
        alivePlayersText' = alivePlayersText game
        deadPlayersText'
            | has (players . traverse . dead) game  = deadPlayersText game
            | otherwise                             = T.empty

playerRescindedVoteMessage :: Text -> Player -> Message
playerRescindedVoteMessage to = privateMessage to . callerRescindedVoteText

engineVersionMessage :: Text -> Message
engineVersionMessage to = privateMessage to engineVersionText

playerMadeDevourVoteMessage :: Text -> Player -> Player -> Message
playerMadeDevourVoteMessage to caller = privateMessage to . callerVotedDevourText caller

playerMadeLynchVoteMessage :: Maybe Text -> Player -> Player -> Message
playerMadeLynchVoteMessage mTo caller = Message mTo . callerVotedLynchText caller