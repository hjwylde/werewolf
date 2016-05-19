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
    playerBootedMessage, villageDrunkJoinedPackMessages, villageDrunkJoinedVillageMessage, playerTurnedToStoneMessage, playerSeenMessage, playerDivinedMessage, playerDevouredMessage, noPlayerDevouredMessage, scapegoatChoseAllowedVotersMessage, playerPoisonedMessage, scapegoatLynchedMessage, playerLynchedMessage, noPlayerLynchedMessage, jesterLynchedMessage, ferinaGruntsMessage, orphanJoinedPackMessages, playerKilledMessage, playerLostMessage, playerContributedMessage, playerWonMessage, witchsTurnMessages, firstWerewolvesTurnMessages, villagesTurnMessage, nightFallsMessage, sunriseMessage, villageDrunksTurnMessage, seersTurnMessages, scapegoatsTurnMessage, protectorsTurnMessages, orphansTurnMessages, oraclesTurnMessages, huntersTurnMessages, stageMessages, fallenAngelMessage, trueVillagerMessage, spitefulGhostMessage, beholderMessage, newPlayerMessage, rolesInGameMessage, newPlayersInGameMessage, newGameMessages, gameOverMessages, fallenAngelWonMessage,
) where

import Control.Lens       hiding (isn't)
import Control.Lens.Extra

import Data.List.Extra
import Data.Text       (Text)

import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role                    hiding (name)
import Game.Werewolf.Variant.Standard.Engine

playerBootedMessage :: Player -> Message
playerBootedMessage = publicMessage . playerBootedText

gameOverMessages :: Game -> [Message]
gameOverMessages game
    | hasFallenAngelWon game    = concat
        [ [fallenAngelWonMessage]
        , [playerRolesMessage game]
        , [playerWonMessage fallenAngelsName]
        , map playerLostMessage (game ^.. players . names \\ [fallenAngelsName])
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

        fallenAngelsName = game ^?! players . fallenAngels . name

fallenAngelWonMessage :: Message
fallenAngelWonMessage = publicMessage fallenAngelWonText

allegianceWonMessage :: Allegiance -> Message
allegianceWonMessage = publicMessage . allegianceWonText

playerRolesMessage :: Game -> Message
playerRolesMessage = publicMessage . playerRolesText

newGameMessages :: Game -> [Message]
newGameMessages game = concat
    [ [newPlayersInGameMessage game]
    , [rolesInGameMessage Nothing game]
    , map newPlayerMessage players'
    , beholderMessages
    , spitefulGhostMessages
    , trueVillagerMessages
    , fallenAngelMessages
    , stageMessages game
    ]
    where
        players'                = game ^. players
        beholderMessages        = case (,) <$> players' ^? beholders <*> players' ^? seers of
            Just (beholder, _)  -> [beholderMessage (beholder ^. name) game]
            _                   -> []
        spitefulGhostMessages   = case players' ^? spitefulGhosts of
            Just spitefulGhost  -> [spitefulGhostMessage (spitefulGhost ^. name) game]
            _                   -> []
        trueVillagerMessages    = [trueVillagerMessage game | has trueVillagers players']
        fallenAngelMessages     = if has fallenAngels players'
            then [fallenAngelMessage]
            else []

newPlayersInGameMessage :: Game -> Message
newPlayersInGameMessage = publicMessage . playersInGameText

rolesInGameMessage :: Maybe Text -> Game -> Message
rolesInGameMessage mTo = Message mTo . rolesInGameText

newPlayerMessage :: Player -> Message
newPlayerMessage player = privateMessage (player ^. name) $ newPlayerText player

beholderMessage :: Text -> Game -> Message
beholderMessage to = privateMessage to . beholderText

spitefulGhostMessage :: Text -> Game -> Message
spitefulGhostMessage to = privateMessage to . spitefulGhostText

trueVillagerMessage :: Game -> Message
trueVillagerMessage = publicMessage . trueVillagerText

fallenAngelMessage :: Message
fallenAngelMessage = publicMessage fallenAngelText

stageMessages :: Game -> [Message]
stageMessages game = case game ^. stage of
    DruidsTurn          -> []
    GameOver            -> []
    HuntersTurn1        -> huntersTurnMessages game
    HuntersTurn2        -> huntersTurnMessages game
    Lynching            -> []
    OraclesTurn         -> oraclesTurnMessages oraclesName
    OrphansTurn         -> orphansTurnMessages orphansName
    ProtectorsTurn      -> protectorsTurnMessages protectorsName
    ScapegoatsTurn      -> [scapegoatsTurnMessage game]
    SeersTurn           -> seersTurnMessages seersName
    Sunrise             -> [sunriseMessage]
    Sunset              -> [nightFallsMessage]
    VillageDrunksTurn   -> [villageDrunksTurnMessage]
    VillagesTurn        -> [villagesTurnMessage]
    WerewolvesTurn      -> if is firstRound game
        then firstWerewolvesTurnMessages game
        else werewolvesTurnMessages aliveWerewolfNames
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

oraclesTurnMessages :: Text -> [Message]
oraclesTurnMessages to =
    [ publicMessage oraclesTurnPublicText
    , privateMessage to oraclesTurnPrivateText
    ]

orphansTurnMessages :: Text -> [Message]
orphansTurnMessages to =
    [ publicMessage orphansTurnPublicText
    , privateMessage to orphansTurnPrivateText
    ]

protectorsTurnMessages :: Text -> [Message]
protectorsTurnMessages to =
    [ publicMessage protectorsTurnPublicText
    , privateMessage to protectorsTurnPrivateText
    ]

scapegoatsTurnMessage :: Game -> Message
scapegoatsTurnMessage = publicMessage . scapegoatsTurnText

seersTurnMessages :: Text -> [Message]
seersTurnMessages to =
    [ publicMessage seersTurnPublicText
    , privateMessage to seersTurnPrivateText
    ]

villageDrunksTurnMessage :: Message
villageDrunksTurnMessage = publicMessage villageDrunksTurnText

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
    ++ werewolvesTurnMessages (map (view name) werewolves)
    where
        werewolves = game ^.. players . traverse . alive . filtered (is werewolf)

werewolvesTurnMessages :: [Text] -> [Message]
werewolvesTurnMessages tos =
    publicMessage werewolvesTurnPublicText
    : groupMessages tos werewolvesTurnPrivateText

witchsTurnMessages :: Game -> [Message]
witchsTurnMessages game = concat
    [ [wakeUpMessage]
    , healMessages
    , poisonMessages
    , [passMessage]
    ]
    where
        to              = game ^?! players . witches . name
        wakeUpMessage   = publicMessage witchsTurnText
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

playerLynchedMessage :: Player -> Message
playerLynchedMessage player
    | is simpleWerewolf player
        || is alphaWolf player  = publicMessage $ werewolfLynchedText player
    | otherwise                 = publicMessage $ playerLynchedText player

scapegoatLynchedMessage :: Game -> Message
scapegoatLynchedMessage = publicMessage . scapegoatLynchedText

scapegoatChoseAllowedVotersMessage :: Game -> Message
scapegoatChoseAllowedVotersMessage = publicMessage . scapegoatsTurnEndedText

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage noPlayerDevouredText

playerDevouredMessage :: Player -> Message
playerDevouredMessage = publicMessage . playerDevouredText

playerDivinedMessage :: Text -> Player -> Message
playerDivinedMessage to = privateMessage to . playerDivinedText

playerPoisonedMessage :: Player -> Message
playerPoisonedMessage = publicMessage . playerPoisonedText

playerSeenMessage :: Text -> Player -> Message
playerSeenMessage to player
    | is alphaWolf player   = privateMessage to $ alphaWolfSeenText player
    | is lycan player       = privateMessage to $ lycanSeenText player
    | otherwise             = privateMessage to $ playerSeenText player

playerTurnedToStoneMessage :: Player -> Message
playerTurnedToStoneMessage = publicMessage . playerTurnedToStoneText

villageDrunkJoinedVillageMessage :: Text -> Message
villageDrunkJoinedVillageMessage to = privateMessage to villageDrunkJoinedVillageText

villageDrunkJoinedPackMessages :: Text -> Game -> [Message]
villageDrunkJoinedPackMessages to game =
    privateMessage to (villageDrunkJoinedWerewolvesPrivateText game)
    : groupMessages (map (view name) werewolves) (villageDrunkJoinedWerewolvesGroupText game)
    where
        villageDrunk    = game ^?! players . villageDrunks
        werewolves      = game ^.. players . traverse . alive . filtered (is werewolf) \\ [villageDrunk]
