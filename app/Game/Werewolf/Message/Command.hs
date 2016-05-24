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
    pingDiurnalRoleMessage, pingNocturnalRoleMessage, pingPlayerMessage, pingVillageMessage,
    pingWerewolvesMessage,

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
import Game.Werewolf.Variant.NoRoleReveal.Command    as NoRoleReveal
import Game.Werewolf.Variant.Standard.Command        as Standard

playerQuitMessage :: Player -> Game -> Message
playerQuitMessage player game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.callerQuitText player
    | otherwise                             = publicMessage $ Standard.callerQuitText player

playerVotedToBootMessage :: Player -> Player -> Message
playerVotedToBootMessage caller = publicMessage . callerVotedBootText caller

circleMessage :: Text -> [Player] -> Message
circleMessage to = privateMessage to . gameCircleText

playerShotMessage :: Player -> Game -> Message
playerShotMessage player game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.playerShotText player
    | otherwise                             = publicMessage $ Standard.playerShotText player

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

pingDiurnalRoleMessage :: Role -> Message
pingDiurnalRoleMessage role = publicMessage $ diurnalRolePingedText role

pingNocturnalRoleMessage :: Role -> Game -> Message
pingNocturnalRoleMessage role game
    | has (variant . _NoRoleKnowledge) game = publicMessage $ NoRoleKnowledge.nocturnalRolePingedText role
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.nocturnalRolePingedText role
    | otherwise                             = publicMessage $ Standard.nocturnalRolePingedText role

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to playerPingedText

pingVillageMessage :: Message
pingVillageMessage = publicMessage villagePingedText

pingWerewolvesMessage :: Game -> Message
pingWerewolvesMessage game
    | has (variant . _NoRoleKnowledge) game = publicMessage NoRoleKnowledge.werewolvesPingedText
    | has (variant . _NoRoleReveal) game    = publicMessage NoRoleReveal.werewolvesPingedText
    | otherwise                             = publicMessage Standard.werewolvesPingedText

currentStageMessage :: Text -> Game -> Message
currentStageMessage to game
    | has (stage . _GameOver) game          = gameIsOverMessage to
    | has (variant . _NoRoleKnowledge) game = privateMessage to $ NoRoleKnowledge.currentTurnText game
    | has (variant . _NoRoleReveal) game    = privateMessage to $ NoRoleReveal.currentTurnText game
    | otherwise                             = privateMessage to $ Standard.currentTurnText game

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to gameOverText

playersInGameMessage :: Text -> Game -> Message
playersInGameMessage to game = privateMessage to $ T.append alivePlayersText' deadPlayersText'
    where
        alivePlayersText' = alivePlayersText game
        deadPlayersText'
            | hasn't (players . traverse . dead) game   = T.empty
            | has (variant . _NoRoleReveal) game        = NoRoleReveal.deadPlayersText game
            | otherwise                                 = Standard.deadPlayersText game

playerRescindedVoteMessage :: Text -> Player -> Message
playerRescindedVoteMessage to = privateMessage to . callerRescindedVoteText

engineVersionMessage :: Text -> Message
engineVersionMessage to = privateMessage to engineVersionText

playerMadeDevourVoteMessage :: Text -> Player -> Player -> Message
playerMadeDevourVoteMessage to caller = privateMessage to . callerVotedDevourText caller

playerMadeLynchVoteMessage :: Maybe Text -> Player -> Player -> Message
playerMadeLynchVoteMessage mTo caller = Message mTo . callerVotedLynchText caller
