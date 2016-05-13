{-|
Module      : Game.Werewolf.Message.Engine
Description : Suite of engine messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of engine messages used throughout the werewolf game.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- TODO (hjw): sort this file
module Game.Werewolf.Message.Engine (
    playerBootedMessage, villageDrunkJoinedPackMessages, villageDrunkJoinedVillageMessage, playerTurnedToStoneMessage, playerSeenMessage, playerDivinedMessage, playerDevouredMessage, noPlayerDevouredMessage, scapegoatChoseAllowedVotersMessage, playerPoisonedMessage, scapegoatLynchedMessage, playerLynchedMessage, noPlayerLynchedMessage, jesterLynchedMessage, ferinaGruntsMessage, orphanJoinedPackMessages, playerKilledMessage, playerLostMessage, playerContributedMessage, playerWonMessage, witchsTurnMessages, firstWerewolvesTurnMessages, villagesTurnMessage, nightFallsMessage, sunriseMessage, villageDrunksTurnMessage, seersTurnMessages, scapegoatsTurnMessage, protectorsTurnMessages, orphansTurnMessages, oraclesTurnMessages, huntersTurnMessages, stageMessages, fallenAngelMessage, trueVillagerMessage, spitefulGhostMessage, beholderMessage, newPlayerMessage, rolesInGameMessage, newPlayersInGameMessage, newGameMessages, gameOverMessages, fallenAngelWonMessage,
) where

import Control.Arrow
import Control.Lens       hiding (isn't)
import Control.Lens.Extra

import           Data.List.Extra
import           Data.String.Humanise
import           Data.String.Interpolate.Extra
import           Data.Text                     (Text)
import qualified Data.Text                     as T

import Game.Werewolf.Game
import Game.Werewolf.Message
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role     hiding (name)

playerBootedMessage :: Player -> Message
playerBootedMessage player = publicMessage [iFile|messages/engine/general/player-booted.text|]

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
fallenAngelWonMessage = publicMessage [iFile|messages/engine/game-over/fallen-angel-won.text|]

allegianceWonMessage :: Allegiance -> Message
allegianceWonMessage allegiance = publicMessage [iFile|messages/engine/game-over/allegiance-won.text|]

playerRolesMessage :: Game -> Message
playerRolesMessage game = publicMessage [iFile|messages/engine/game-over/player-roles.text|]

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
            Just (beholder, seer)   -> [beholderMessage (beholder ^. name) seer]
            _                       -> []
        spitefulGhostMessages   = case players' ^? spitefulGhosts of
            Just spitefulGhost  -> [spitefulGhostMessage (spitefulGhost ^. name) game]
            _                   -> []
        trueVillagerMessages    = case players' ^? trueVillagers of
            Just trueVillager   -> [trueVillagerMessage trueVillager]
            _                   -> []
        fallenAngelMessages     = if has fallenAngels players'
            then [fallenAngelMessage]
            else []

newPlayersInGameMessage :: Game -> Message
newPlayersInGameMessage game = publicMessage [iFile|messages/engine/new-game/players-in-game.text|]

rolesInGameMessage :: Maybe Text -> Game -> Message
rolesInGameMessage mTo game = Message mTo [iFile|messages/engine/new-game/roles-in-game.text|]
    where
        roles           = game ^.. players . traverse . role
        roleCounts      = map (head &&& length) (groupSortOn humanise roles)
        totalBalance    = sumOf (traverse . balance) roles

newPlayerMessage :: Player -> Message
newPlayerMessage player = privateMessage (player ^. name) [iFile|messages/engine/new-game/new-player.text|]

beholderMessage :: Text -> Player -> Message
beholderMessage to seer = privateMessage to [iFile|messages/engine/new-game/beholder.text|]

spitefulGhostMessage :: Text -> Game -> Message
spitefulGhostMessage to game = privateMessage to [iFile|messages/engine/new-game/spiteful-ghost.text|]

trueVillagerMessage :: Player -> Message
trueVillagerMessage trueVillager = publicMessage [iFile|messages/engine/new-game/true-villager.text|]

fallenAngelMessage :: Message
fallenAngelMessage = publicMessage [iFile|messages/engine/new-game/fallen-angel.text|]

stageMessages :: Game -> [Message]
stageMessages game = case game ^. stage of
    DruidsTurn          -> []
    GameOver            -> []
    HuntersTurn1        -> huntersTurnMessages hunter
    HuntersTurn2        -> huntersTurnMessages hunter
    Lynching            -> []
    OraclesTurn         -> oraclesTurnMessages oraclesName
    OrphansTurn         -> orphansTurnMessages orphansName
    ProtectorsTurn      -> protectorsTurnMessages protectorsName
    ScapegoatsTurn      -> [scapegoatsTurnMessage scapegoat]
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
        hunter              = players' ^?! hunters
        oraclesName         = players' ^?! oracles . name
        orphansName         = players' ^?! orphans . name
        protectorsName      = players' ^?! protectors . name
        scapegoat           = players' ^?! scapegoats
        seersName           = players' ^?! seers . name
        aliveWerewolfNames  = players' ^.. werewolves . alive . name

huntersTurnMessages :: Player -> [Message]
huntersTurnMessages hunter =
    [ publicMessage [iFile|messages/engine/hunters-turn/start-public.text|]
    , privateMessage (hunter ^. name) [iFile|messages/engine/hunters-turn/start-private.text|]
    ]

oraclesTurnMessages :: Text -> [Message]
oraclesTurnMessages to =
    [ publicMessage [iFile|messages/engine/oracles-turn/start-public.text|]
    , privateMessage to [iFile|messages/engine/oracles-turn/start-private.text|]
    ]

orphansTurnMessages :: Text -> [Message]
orphansTurnMessages to =
    [ publicMessage [iFile|messages/engine/orphans-turn/start-public.text|]
    , privateMessage to [iFile|messages/engine/orphans-turn/start-private.text|]
    ]

protectorsTurnMessages :: Text -> [Message]
protectorsTurnMessages to =
    [ publicMessage [iFile|messages/engine/protectors-turn/start-public.text|]
    , privateMessage to [iFile|messages/engine/protectors-turn/start-private.text|]
    ]

scapegoatsTurnMessage :: Player -> Message
scapegoatsTurnMessage scapegoat = publicMessage [iFile|messages/engine/scapegoats-turn/start.text|]

seersTurnMessages :: Text -> [Message]
seersTurnMessages to =
    [ publicMessage [iFile|messages/engine/seers-turn/start-public.text|]
    , privateMessage to [iFile|messages/engine/seers-turn/start-private.text|]
    ]

villageDrunksTurnMessage :: Message
villageDrunksTurnMessage = publicMessage [iFile|messages/engine/village-drunks-turn/start.text|]

sunriseMessage :: Message
sunriseMessage = publicMessage [iFile|messages/engine/sunrise/start.text|]

nightFallsMessage :: Message
nightFallsMessage = publicMessage [iFile|messages/engine/sunset/start.text|]

villagesTurnMessage :: Message
villagesTurnMessage = publicMessage [iFile|messages/engine/villages-turn/start.text|]

firstWerewolvesTurnMessages :: Game -> [Message]
firstWerewolvesTurnMessages game =
    [privateMessage (player ^. name) [iFile|messages/engine/werewolves-turn/start-first-round-private.text|]
        | length werewolves > 1, player <- werewolves]
    ++ werewolvesTurnMessages (map (view name) werewolves)
    where
        werewolves = game ^.. players . traverse . alive . filtered (is werewolf)

werewolvesTurnMessages :: [Text] -> [Message]
werewolvesTurnMessages tos =
    publicMessage [iFile|messages/engine/werewolves-turn/start-public.text|]
    : groupMessages tos [iFile|messages/engine/werewolves-turn/start-private.text|]

witchsTurnMessages :: Game -> [Message]
witchsTurnMessages game = concat
    [ [wakeUpMessage]
    , healMessages
    , poisonMessages
    , [passMessage]
    ]
    where
        to              = game ^?! players . witches . name
        victim          = game ^?! votee
        wakeUpMessage   = publicMessage [iFile|messages/engine/witchs-turn/start-public.text|]
        passMessage     = privateMessage to [iFile|messages/engine/witchs-turn/start-pass.text|]
        healMessages
            | game ^. healUsed  = []
            | hasn't votee game = []
            | otherwise         = [privateMessage to [iFile|messages/engine/witchs-turn/start-heal.text|]]
        poisonMessages
            | game ^. poisonUsed    = []
            | otherwise             = [privateMessage to [iFile|messages/engine/witchs-turn/start-poison.text|]]

playerWonMessage :: Text -> Message
playerWonMessage to = privateMessage to [iFile|messages/engine/game-over/player-won.text|]

playerContributedMessage :: Text -> Message
playerContributedMessage to = privateMessage to [iFile|messages/engine/game-over/player-contributed.text|]

playerLostMessage :: Text -> Message
playerLostMessage to = privateMessage to [iFile|messages/engine/game-over/player-lost.text|]

playerKilledMessage :: Text -> Message
playerKilledMessage to = privateMessage to [iFile|messages/engine/general/player-killed.text|]

orphanJoinedPackMessages :: Text -> Game -> [Message]
orphanJoinedPackMessages to game =
    privateMessage to [iFile|messages/engine/orphans-turn/player-joined-werewolves-private.text|]
    : groupMessages (map (view name) werewolves) [iFile|messages/engine/orphans-turn/player-joined-werewolves-group.text|]
    where
        orphan      = game ^?! players . villageDrunks
        werewolves  = game ^.. players . traverse . alive . filtered (is werewolf) \\ [orphan]

ferinaGruntsMessage :: Message
ferinaGruntsMessage = publicMessage [iFile|messages/engine/druids-turn/start.text|]

jesterLynchedMessage :: Player -> Message
jesterLynchedMessage jester = publicMessage [iFile|messages/engine/lynching/jester-lynched.text|]

noPlayerLynchedMessage :: Message
noPlayerLynchedMessage = publicMessage [iFile|messages/engine/lynching/no-player-lynched.text|]

playerLynchedMessage :: Player -> Message
playerLynchedMessage player
    | is simpleWerewolf player
        || is alphaWolf player  = publicMessage [iFile|messages/engine/lynching/werewolf-lynched.text|]
    | otherwise                 = publicMessage [iFile|messages/engine/lynching/player-lynched.text|]

scapegoatLynchedMessage :: Player -> Message
scapegoatLynchedMessage scapegoat = publicMessage [iFile|messages/engine/lynching/scapegoat-lynched.text|]

scapegoatChoseAllowedVotersMessage :: Game -> Message
scapegoatChoseAllowedVotersMessage game = publicMessage [iFile|messages/engine/scapegoats-turn/end.text|]

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage [iFile|messages/engine/sunrise/no-player-devoured.text|]

playerDevouredMessage :: Player -> Message
playerDevouredMessage player = publicMessage [iFile|messages/engine/sunrise/player-devoured.text|]

playerDivinedMessage :: Text -> Player -> Message
playerDivinedMessage to player = privateMessage to [iFile|messages/engine/sunrise/player-divined.text|]

playerPoisonedMessage :: Player -> Message
playerPoisonedMessage player = publicMessage [iFile|messages/engine/sunrise/player-poisoned.text|]

playerSeenMessage :: Text -> Player -> Message
playerSeenMessage to player
    | is alphaWolf player   = privateMessage to [iFile|messages/engine/sunrise/alpha-wolf-seen.text|]
    | is lycan player       = privateMessage to [iFile|messages/engine/sunrise/lycan-seen.text|]
    | otherwise             = privateMessage to [iFile|messages/engine/sunrise/player-seen.text|]
    where
        article = if is loner player then "" else "the " :: Text

playerTurnedToStoneMessage :: Player -> Message
playerTurnedToStoneMessage player = publicMessage [iFile|messages/engine/sunrise/player-turned-to-stone.text|]

villageDrunkJoinedVillageMessage :: Text -> Message
villageDrunkJoinedVillageMessage to = privateMessage to [iFile|messages/engine/village-drunks-turn/player-joined-village.text|]

villageDrunkJoinedPackMessages :: Text -> Game -> [Message]
villageDrunkJoinedPackMessages to game =
    privateMessage to [iFile|messages/engine/village-drunks-turn/player-joined-werewolves-private.text|]
    : groupMessages (map (view name) werewolves) [iFile|messages/engine/village-drunks-turn/player-joined-werewolves-group.text|]
    where
        villageDrunk    = game ^?! players . villageDrunks
        werewolves      = game ^.. players . traverse . alive . filtered (is werewolf) \\ [villageDrunk]
