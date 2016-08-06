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
    necromancerCommandsMessage, oracleCommandsMessage, orphanCommandsMessage,
    protectorCommandsMessage, roleMessage, rulesMessage, scapegoatCommandsMessage,
    seerCommandsMessage, stagesMessage, standardCommandsMessage, statusCommandsMessage,
    witchCommandsMessage,

    -- * Ping
    pingDiurnalRoleMessage, pingNocturnalRoleMessage, pingPlayerMessage, pingVillageMessage,
    pingWerewolvesMessage,

    -- * Raise
    deadRaisedMessages,

    -- * Status
    currentDiurnalTurnMessage, currentNocturnalTurnMessage, gameIsOverMessage, marksInGameMessage,
    playersInGameMessage,

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
import Game.Werewolf.Role                            hiding (name)
import Game.Werewolf.Variant                            hiding (name)
import Game.Werewolf.Variant.NoRoleKnowledge.Command as NoRoleKnowledge
import Game.Werewolf.Variant.NoRoleReveal.Command    as NoRoleReveal
import Game.Werewolf.Variant.Standard.Command        as Standard

playerQuitMessage :: Player -> Game -> Message
playerQuitMessage player game
    | has (variant . noRoleReveal) game = publicMessage $ NoRoleReveal.callerQuitText player
    | otherwise                         = publicMessage $ Standard.callerQuitText player

playerVotedToBootMessage :: Player -> Player -> Message
playerVotedToBootMessage caller = publicMessage . callerVotedBootText caller

circleMessage :: Text -> [Player] -> Message
circleMessage to = privateMessage to . gameCircleText

playerShotMessage :: Player -> Game -> Message
playerShotMessage player game
    | has (variant . noRoleReveal) game = publicMessage $ NoRoleReveal.playerShotText player
    | otherwise                         = publicMessage $ Standard.playerShotText player

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

necromancerCommandsMessage :: Text -> Message
necromancerCommandsMessage to = privateMessage to necromancerCommandsText

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
    ] ++ [ necromancersTurnText
    | isNothing mGame || has (players . necromancers . named to) (fromJust mGame)
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
    | has (variant . noRoleKnowledge) game  = publicMessage $ NoRoleKnowledge.nocturnalRolePingedText role
    | has (variant . noRoleReveal) game     = publicMessage $ NoRoleReveal.nocturnalRolePingedText role
    | otherwise                             = publicMessage $ Standard.nocturnalRolePingedText role

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to playerPingedText

pingVillageMessage :: Message
pingVillageMessage = publicMessage villagePingedText

pingWerewolvesMessage :: Game -> Message
pingWerewolvesMessage game
    | has (variant . noRoleKnowledge) game  = publicMessage NoRoleKnowledge.werewolvesPingedText
    | has (variant . noRoleReveal) game     = publicMessage NoRoleReveal.werewolvesPingedText
    | otherwise                             = publicMessage Standard.werewolvesPingedText

deadRaisedMessages :: Game -> [Message]
deadRaisedMessages game =
    necromancerRaisedDeadMessage game
    : map (`playerRaisedFromDeadMessage` game) zombieNames
    where
        zombieNames = game ^.. players . zombies . name

necromancerRaisedDeadMessage :: Game -> Message
necromancerRaisedDeadMessage = publicMessage . necromancerRaisedDeadText

playerRaisedFromDeadMessage :: Text -> Game -> Message
playerRaisedFromDeadMessage to = privateMessage to . playerRaisedFromDeadText

currentDiurnalTurnMessage :: Text -> Game -> Message
currentDiurnalTurnMessage to game = privateMessage to $ Standard.currentDiurnalTurnText game

currentNocturnalTurnMessage :: Text -> Game -> Message
currentNocturnalTurnMessage to game
    | has (variant . noRoleKnowledge) game  = privateMessage to $ NoRoleKnowledge.currentNocturnalTurnText game
    | has (variant . noRoleReveal) game     = privateMessage to $ NoRoleReveal.currentNocturnalTurnText game
    | otherwise                             = privateMessage to $ Standard.currentNocturnalTurnText game

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to gameOverText

marksInGameMessage :: Text -> Game -> Message
marksInGameMessage to game = privateMessage to $ marksText game

playersInGameMessage :: Text -> Game -> Message
playersInGameMessage to game = privateMessage to $ T.append alivePlayersText' deadPlayersText'
    where
        alivePlayersText' = alivePlayersText game
        deadPlayersText'
            | hasn't (players . traverse . dead) game   = T.empty
            | has (variant . noRoleReveal) game         = NoRoleReveal.deadPlayersText game
            | otherwise                                 = Standard.deadPlayersText game

playerRescindedVoteMessage :: Text -> Player -> Message
playerRescindedVoteMessage to = privateMessage to . callerRescindedVoteText

engineVersionMessage :: Text -> Message
engineVersionMessage to = privateMessage to engineVersionText

playerMadeDevourVoteMessage :: Text -> Player -> Player -> Message
playerMadeDevourVoteMessage to caller = privateMessage to . callerVotedDevourText caller

playerMadeLynchVoteMessage :: Maybe Text -> Player -> Player -> Message
playerMadeLynchVoteMessage mTo caller = Message mTo . callerVotedLynchText caller
