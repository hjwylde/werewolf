{-|
Module      : Game.Werewolf.Message.Engine
Description : Suite of engine messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of engine messages used throughout the werewolf game.
-}

-- TODO (hjw): sort this file
module Game.Werewolf.Message.Engine (
    playerRolesMessage, playerBootedMessage, villageDrunkJoinedPackMessages, villageDrunkJoinedVillageMessage,
    playerTurnedToStoneMessage, playerSeenMessage, playerDivinedMessage, playerDevouredMessage,
    noPlayerDevouredMessage, scapegoatChoseAllowedVotersMessage, playerPoisonedMessage,
    scapegoatLynchedMessage, saintLynchedMessage, playerLynchedMessage, noPlayerLynchedMessage, jesterLynchedMessage,
    ferinaGruntsMessage, orphanJoinedPackMessages, playerKilledMessage, playerLostMessage,
    playerContributedMessage, playerWonMessage, witchsTurnMessages, firstWerewolvesTurnMessages,
    villagesTurnMessage, nightFallsMessage, sunriseMessage, villageDrunksTurnMessages, spitefulGhostKilledMessage,
    seersTurnMessages, scapegoatsTurnMessage, protectorsTurnMessages, orphansTurnMessages,
    oraclesTurnMessages, huntersTurnMessages, stageMessages,
    trueVillagerMessage, beholderMessage, newPlayerMessage, rolesInGameMessage,
    newPlayersInGameMessage, newGameMessages, gameOverMessages, fallenAngelWonMessage,
    werewolfLynchedMessage,
) where

import Control.Lens.Extra

import Data.List.Extra
import Data.Text       (Text)

import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role                           hiding (name)
import Game.Werewolf.Variant.NoRoleKnowledge.Engine as NoRoleKnowledge
import Game.Werewolf.Variant.NoRoleReveal.Engine    as NoRoleReveal
import Game.Werewolf.Variant.Standard.Engine        as Standard

playerBootedMessage :: Player -> Game -> Message
playerBootedMessage player game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.playerBootedText player
    | otherwise                             = publicMessage $ Standard.playerBootedText player

gameOverMessages :: Game -> [Message]
gameOverMessages game
    | hasDullahanWon game    = concat
        [ [dullahanWonMessage game]
        , [playerRolesMessage game]
        , [playerWonMessage dullahansName]
        , map playerLostMessage (game ^.. players . names \\ [dullahansName])
        ]
    | hasFallenAngelWon game    = concat
        [ [fallenAngelWonMessage]
        , [playerRolesMessage game]
        , [playerWonMessage fallenAngelsName]
        , map playerLostMessage (game ^.. players . names \\ [fallenAngelsName])
        ]
    | hasEveryoneLost game      = concat
        [ [everyoneLostMessage]
        , [playerRolesMessage game]
        , map playerLostMessage (game ^.. players . names)
        ]
    | otherwise                 = concat
        [ [allegianceWonMessage winningAllegiance]
        , [playerRolesMessage game]
        , playerWonMessages
        , playerContributedMessages
        , playerLostMessages
        ]
    where
        winningAllegiance
            | hasVillagersWon game      = Villagers
            | hasWerewolvesWon game     = Werewolves
            | otherwise                 = undefined

        winningPlayers  = game ^.. players . traverse . filteredBy (role . allegiance) winningAllegiance
        losingPlayers   = game ^. players \\ winningPlayers

        playerWonMessages           = map playerWonMessage (winningPlayers ^.. traverse . alive . name)
        playerContributedMessages   = map playerContributedMessage (winningPlayers ^.. traverse . dead . name)
        playerLostMessages          = map playerLostMessage (losingPlayers ^.. names)

        dullahansName       = game ^?! players . dullahans . name
        fallenAngelsName    = game ^?! players . fallenAngels . name

dullahanWonMessage :: Game -> Message
dullahanWonMessage = publicMessage . dullahanWonText

everyoneLostMessage :: Message
everyoneLostMessage = publicMessage everyoneLostText

fallenAngelWonMessage :: Message
fallenAngelWonMessage = publicMessage fallenAngelWonText

allegianceWonMessage :: Allegiance -> Message
allegianceWonMessage = publicMessage . allegianceWonText

playerRolesMessage :: Game -> Message
playerRolesMessage = publicMessage . playerRolesText

newGameMessages :: Game -> [Message]
newGameMessages game = concat
    [ [newPlayersInGameMessage game]
    , gameVariantMessages
    , [rolesInGameMessage Nothing game]
    , map newPlayerMessage players'
    , beholderMessages
    , dullahanMessages
    , trueVillagerMessages
    , stageMessages game
    ]
    where
        players'                = game ^. players
        gameVariantMessages     =
            [ gameVariantMessage game
            | hasn't (variant . _Standard) game
            ]
        beholderMessages        =
            [ beholderMessage beholderName game
            | has beholders players'
            , has seers players'
            , beholderName <- players' ^.. beholders . name
            ]
        dullahanMessages        =
            [ dullahanMessage dullahanName game
            | has dullahans players'
            , dullahanName <- players' ^.. dullahans . name
            ]
        trueVillagerMessages    =
            [ trueVillagerMessage game
            | has trueVillagers players'
            ]

newPlayersInGameMessage :: Game -> Message
newPlayersInGameMessage = publicMessage . playersInGameText

gameVariantMessage :: Game -> Message
gameVariantMessage = publicMessage . gameVariantText

rolesInGameMessage :: Maybe Text -> Game -> Message
rolesInGameMessage mTo game
    | has (variant . _NoRoleKnowledge) game = Message mTo $ NoRoleKnowledge.rolesInGameText game
    | otherwise                             = Message mTo $ Standard.rolesInGameText game

newPlayerMessage :: Player -> Message
newPlayerMessage player = privateMessage (player ^. name) $ newPlayerText player

beholderMessage :: Text -> Game -> Message
beholderMessage to = privateMessage to . beholderText

dullahanMessage :: Text -> Game -> Message
dullahanMessage to = privateMessage to . dullahanText

spitefulGhostKilledMessage :: Text -> Game -> Message
spitefulGhostKilledMessage to game
    | has (variant . _NoRoleReveal) game    = privateMessage to $ NoRoleReveal.spitefulGhostKilledText game
    | otherwise                             = privateMessage to $ Standard.spitefulGhostKilledText game

trueVillagerMessage :: Game -> Message
trueVillagerMessage = publicMessage . trueVillagerText

stageMessages :: Game -> [Message]
stageMessages game = case game ^. stage of
    DruidsTurn          -> []
    GameOver            -> []
    HuntersTurn1        -> huntersTurnMessages game
    HuntersTurn2        -> huntersTurnMessages game
    Lynching            -> []
    OraclesTurn         -> oraclesTurnMessages oraclesName game
    OrphansTurn         -> orphansTurnMessages orphansName game
    ProtectorsTurn      -> protectorsTurnMessages protectorsName game
    ScapegoatsTurn      -> [scapegoatsTurnMessage game]
    SeersTurn           -> seersTurnMessages seersName game
    Sunrise             -> [sunriseMessage]
    Sunset              -> [nightFallsMessage]
    VillageDrunksTurn   -> villageDrunksTurnMessages game
    VillagesTurn        -> [villagesTurnMessage]
    WerewolvesTurn      -> if is firstRound game
        then firstWerewolvesTurnMessages game
        else werewolvesTurnMessages aliveWerewolfNames game
    WitchsTurn          -> witchsTurnMessages game
    where
        players'            = game ^. players
        oraclesName         = players' ^?! oracles . name
        orphansName         = players' ^?! orphans . name
        protectorsName      = players' ^?! protectors . name
        seersName           = players' ^?! seers . name
        aliveWerewolfNames  = players' ^.. werewolves . alive . name

huntersTurnMessages :: Game -> [Message]
huntersTurnMessages game =
    [ publicMessage $ huntersTurnPublicText game
    , privateMessage hunterName huntersTurnPrivateText
    ]
    where
        hunterName = game ^?! players . hunters . name

oraclesTurnMessages :: Text -> Game -> [Message]
oraclesTurnMessages to game
    | has (variant . _NoRoleKnowledge) game =
        [ privateMessage to oraclesTurnPrivateText ]
    | has (variant . _NoRoleReveal) game    =
        [ privateMessage to oraclesTurnPrivateText ]
    | otherwise                             =
        [ publicMessage oraclesTurnPublicText
        , privateMessage to oraclesTurnPrivateText
        ]

orphansTurnMessages :: Text -> Game -> [Message]
orphansTurnMessages to game
    | has (variant . _NoRoleKnowledge) game =
        [ privateMessage to orphansTurnPrivateText ]
    | has (variant . _NoRoleReveal) game    =
        [ privateMessage to orphansTurnPrivateText ]
    | otherwise                             =
        [ publicMessage orphansTurnPublicText
        , privateMessage to orphansTurnPrivateText
        ]

protectorsTurnMessages :: Text -> Game -> [Message]
protectorsTurnMessages to game
    | has (variant . _NoRoleKnowledge) game =
        [ privateMessage to protectorsTurnPrivateText ]
    | has (variant . _NoRoleReveal) game    =
        [ privateMessage to protectorsTurnPrivateText ]
    | otherwise                             =
        [ publicMessage protectorsTurnPublicText
        , privateMessage to protectorsTurnPrivateText
        ]

scapegoatsTurnMessage :: Game -> Message
scapegoatsTurnMessage = publicMessage . scapegoatsTurnText

seersTurnMessages :: Text -> Game -> [Message]
seersTurnMessages to game
    | has (variant . _NoRoleKnowledge) game =
        [ privateMessage to seersTurnPrivateText ]
    | has (variant . _NoRoleReveal) game    =
        [ privateMessage to seersTurnPrivateText ]
    | otherwise                             =
        [ publicMessage seersTurnPublicText
        , privateMessage to seersTurnPrivateText
        ]

villageDrunksTurnMessages :: Game -> [Message]
villageDrunksTurnMessages game
    | has (variant . _NoRoleKnowledge) game = []
    | has (variant . _NoRoleReveal) game    = []
    | otherwise                             = [publicMessage villageDrunksTurnText]

sunriseMessage :: Message
sunriseMessage = publicMessage sunriseText

nightFallsMessage :: Message
nightFallsMessage = publicMessage sunsetText

villagesTurnMessage :: Message
villagesTurnMessage = publicMessage villagesTurnText

firstWerewolvesTurnMessages :: Game -> [Message]
firstWerewolvesTurnMessages game =
    [privateMessage (player ^. name) (firstWerewolvesTurnText player game)
        | length werewolves > 1, player <- werewolves]
    ++ werewolvesTurnMessages (map (view name) werewolves) game
    where
        werewolves = game ^.. players . traverse . alive . filtered (is werewolf)

werewolvesTurnMessages :: [Text] -> Game -> [Message]
werewolvesTurnMessages tos game
    | has (variant . _NoRoleKnowledge) game =
        groupMessages tos werewolvesTurnPrivateText
    | has (variant . _NoRoleReveal) game    =
        groupMessages tos werewolvesTurnPrivateText
    | otherwise                             =
        publicMessage werewolvesTurnPublicText
        : groupMessages tos werewolvesTurnPrivateText

witchsTurnMessages :: Game -> [Message]
witchsTurnMessages game = concat
    [ wakeUpMessages
    , healMessages
    , poisonMessages
    , [passMessage]
    ]
    where
        to              = game ^?! players . witches . name
        wakeUpMessages
            | has (variant . _NoRoleKnowledge) game = []
            | has (variant . _NoRoleReveal) game    = []
            | otherwise                             = [publicMessage witchsTurnText]
        passMessage     = privateMessage to passText
        healMessages
            | game ^. healUsed  = []
            | hasn't votee game = []
            | otherwise         = [privateMessage to $ healText game]
        poisonMessages
            | game ^. poisonUsed    = []
            | otherwise             = [privateMessage to poisonText]

playerWonMessage :: Text -> Message
playerWonMessage to = privateMessage to playerWonText

playerContributedMessage :: Text -> Message
playerContributedMessage to = privateMessage to playerContributedText

playerLostMessage :: Text -> Message
playerLostMessage to = privateMessage to playerLostText

playerKilledMessage :: Text -> Message
playerKilledMessage to = privateMessage to playerKilledText

orphanJoinedPackMessages :: Text -> Game -> [Message]
orphanJoinedPackMessages to game =
    privateMessage to (orphanJoinedWerewolvesPrivateText game)
    : groupMessages (map (view name) werewolves) (orphanJoinedWerewolvesGroupText game)
    where
        orphan      = game ^?! players . villageDrunks
        werewolves  = game ^.. players . traverse . alive . filtered (is werewolf) \\ [orphan]

ferinaGruntsMessage :: Message
ferinaGruntsMessage = publicMessage druidsTurnText

jesterLynchedMessage :: Game -> Message
jesterLynchedMessage = publicMessage . jesterLynchedText

noPlayerLynchedMessage :: Message
noPlayerLynchedMessage = publicMessage noPlayerLynchedText

werewolfLynchedMessage :: Player -> Game -> Message
werewolfLynchedMessage player game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.playerLynchedText player
    | otherwise                             = publicMessage $ Standard.werewolfLynchedText player

playerLynchedMessage :: Player -> Game -> Message
playerLynchedMessage player game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.playerLynchedText player
    | otherwise                             = publicMessage $ Standard.playerLynchedText player

saintLynchedMessage :: [Player] -> Game -> Message
saintLynchedMessage voters game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.saintLynchedText voters
    | otherwise                             = publicMessage $ Standard.saintLynchedText voters

scapegoatLynchedMessage :: Game -> Message
scapegoatLynchedMessage = publicMessage . scapegoatLynchedText

scapegoatChoseAllowedVotersMessage :: Game -> Message
scapegoatChoseAllowedVotersMessage = publicMessage . scapegoatsTurnEndedText

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage noPlayerDevouredText

playerDevouredMessage :: Player -> Game -> Message
playerDevouredMessage player game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.playerDevouredText player
    | otherwise                             = publicMessage $ Standard.playerDevouredText player

playerDivinedMessage :: Text -> Player -> Message
playerDivinedMessage to = privateMessage to . playerDivinedText

playerPoisonedMessage :: Player -> Game -> Message
playerPoisonedMessage player game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.playerPoisonedText player
    | otherwise                             = publicMessage $ Standard.playerPoisonedText player

playerSeenMessage :: Text -> Player -> Message
playerSeenMessage to player
    | is alphaWolf player   = privateMessage to $ alphaWolfSeenText player
    | is lycan player       = privateMessage to $ lycanSeenText player
    | otherwise             = privateMessage to $ playerSeenText player

playerTurnedToStoneMessage :: Player -> Game -> Message
playerTurnedToStoneMessage player game
    | has (variant . _NoRoleReveal) game    = publicMessage $ NoRoleReveal.playerTurnedToStoneText player
    | otherwise                             = publicMessage $ Standard.playerTurnedToStoneText player

villageDrunkJoinedVillageMessage :: Text -> Message
villageDrunkJoinedVillageMessage to = privateMessage to villageDrunkJoinedVillageText

villageDrunkJoinedPackMessages :: Text -> Game -> [Message]
villageDrunkJoinedPackMessages to game =
    privateMessage to (villageDrunkJoinedWerewolvesPrivateText game)
    : groupMessages (map (view name) werewolves) (villageDrunkJoinedWerewolvesGroupText game)
    where
        villageDrunk    = game ^?! players . villageDrunks
        werewolves      = game ^.. players . traverse . alive . filtered (is werewolf) \\ [villageDrunk]
