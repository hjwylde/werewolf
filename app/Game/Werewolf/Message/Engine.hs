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
playerBootedMessage player = publicMessage [iFile|variant/standard/engine/general/player-booted.txt|]

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
fallenAngelWonMessage = publicMessage [iFile|variant/standard/engine/game-over/fallen-angel-won.txt|]

allegianceWonMessage :: Allegiance -> Message
allegianceWonMessage allegiance = publicMessage [iFile|variant/standard/engine/game-over/allegiance-won.txt|]

playerRolesMessage :: Game -> Message
playerRolesMessage game = publicMessage [iFile|variant/standard/engine/game-over/player-roles.txt|]

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
newPlayersInGameMessage game = publicMessage [iFile|variant/standard/engine/new-game/players-in-game.txt|]

rolesInGameMessage :: Maybe Text -> Game -> Message
rolesInGameMessage mTo game = Message mTo [iFile|variant/standard/engine/new-game/roles-in-game.txt|]
    where
        roles           = game ^.. players . traverse . role
        roleCounts      = map (head &&& length) (groupSortOn humanise roles)
        totalBalance    = sumOf (traverse . balance) roles

newPlayerMessage :: Player -> Message
newPlayerMessage player = privateMessage (player ^. name) [iFile|variant/standard/engine/new-game/new-player.txt|]

beholderMessage :: Text -> Player -> Message
beholderMessage to seer = privateMessage to [iFile|variant/standard/engine/new-game/beholder.txt|]

spitefulGhostMessage :: Text -> Game -> Message
spitefulGhostMessage to game = privateMessage to [iFile|variant/standard/engine/new-game/spiteful-ghost.txt|]

trueVillagerMessage :: Player -> Message
trueVillagerMessage trueVillager = publicMessage [iFile|variant/standard/engine/new-game/true-villager.txt|]

fallenAngelMessage :: Message
fallenAngelMessage = publicMessage [iFile|variant/standard/engine/new-game/fallen-angel.txt|]

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
    [ publicMessage [iFile|variant/standard/engine/hunters-turn/start-public.txt|]
    , privateMessage (hunter ^. name) [iFile|variant/standard/engine/hunters-turn/start-private.txt|]
    ]

oraclesTurnMessages :: Text -> [Message]
oraclesTurnMessages to =
    [ publicMessage [iFile|variant/standard/engine/oracles-turn/start-public.txt|]
    , privateMessage to [iFile|variant/standard/engine/oracles-turn/start-private.txt|]
    ]

orphansTurnMessages :: Text -> [Message]
orphansTurnMessages to =
    [ publicMessage [iFile|variant/standard/engine/orphans-turn/start-public.txt|]
    , privateMessage to [iFile|variant/standard/engine/orphans-turn/start-private.txt|]
    ]

protectorsTurnMessages :: Text -> [Message]
protectorsTurnMessages to =
    [ publicMessage [iFile|variant/standard/engine/protectors-turn/start-public.txt|]
    , privateMessage to [iFile|variant/standard/engine/protectors-turn/start-private.txt|]
    ]

scapegoatsTurnMessage :: Player -> Message
scapegoatsTurnMessage scapegoat = publicMessage [iFile|variant/standard/engine/scapegoats-turn/start.txt|]

seersTurnMessages :: Text -> [Message]
seersTurnMessages to =
    [ publicMessage [iFile|variant/standard/engine/seers-turn/start-public.txt|]
    , privateMessage to [iFile|variant/standard/engine/seers-turn/start-private.txt|]
    ]

villageDrunksTurnMessage :: Message
villageDrunksTurnMessage = publicMessage [iFile|variant/standard/engine/village-drunks-turn/start.txt|]

sunriseMessage :: Message
sunriseMessage = publicMessage [iFile|variant/standard/engine/sunrise/start.txt|]

nightFallsMessage :: Message
nightFallsMessage = publicMessage [iFile|variant/standard/engine/sunset/start.txt|]

villagesTurnMessage :: Message
villagesTurnMessage = publicMessage [iFile|variant/standard/engine/villages-turn/start.txt|]

firstWerewolvesTurnMessages :: Game -> [Message]
firstWerewolvesTurnMessages game =
    [privateMessage (player ^. name) [iFile|variant/standard/engine/werewolves-turn/start-first-round-private.txt|]
        | length werewolves > 1, player <- werewolves]
    ++ werewolvesTurnMessages (map (view name) werewolves)
    where
        werewolves = game ^.. players . traverse . alive . filtered (is werewolf)

werewolvesTurnMessages :: [Text] -> [Message]
werewolvesTurnMessages tos =
    publicMessage [iFile|variant/standard/engine/werewolves-turn/start-public.txt|]
    : groupMessages tos [iFile|variant/standard/engine/werewolves-turn/start-private.txt|]

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
        wakeUpMessage   = publicMessage [iFile|variant/standard/engine/witchs-turn/start-public.txt|]
        passMessage     = privateMessage to [iFile|variant/standard/engine/witchs-turn/start-pass.txt|]
        healMessages
            | game ^. healUsed  = []
            | hasn't votee game = []
            | otherwise         = [privateMessage to [iFile|variant/standard/engine/witchs-turn/start-heal.txt|]]
        poisonMessages
            | game ^. poisonUsed    = []
            | otherwise             = [privateMessage to [iFile|variant/standard/engine/witchs-turn/start-poison.txt|]]

playerWonMessage :: Text -> Message
playerWonMessage to = privateMessage to [iFile|variant/standard/engine/game-over/player-won.txt|]

playerContributedMessage :: Text -> Message
playerContributedMessage to = privateMessage to [iFile|variant/standard/engine/game-over/player-contributed.txt|]

playerLostMessage :: Text -> Message
playerLostMessage to = privateMessage to [iFile|variant/standard/engine/game-over/player-lost.txt|]

playerKilledMessage :: Text -> Message
playerKilledMessage to = privateMessage to [iFile|variant/standard/engine/general/player-killed.txt|]

orphanJoinedPackMessages :: Text -> Game -> [Message]
orphanJoinedPackMessages to game =
    privateMessage to [iFile|variant/standard/engine/orphans-turn/player-joined-werewolves-private.txt|]
    : groupMessages (map (view name) werewolves) [iFile|variant/standard/engine/orphans-turn/player-joined-werewolves-group.txt|]
    where
        orphan      = game ^?! players . villageDrunks
        werewolves  = game ^.. players . traverse . alive . filtered (is werewolf) \\ [orphan]

ferinaGruntsMessage :: Message
ferinaGruntsMessage = publicMessage [iFile|variant/standard/engine/druids-turn/start.txt|]

jesterLynchedMessage :: Player -> Message
jesterLynchedMessage jester = publicMessage [iFile|variant/standard/engine/lynching/jester-lynched.txt|]

noPlayerLynchedMessage :: Message
noPlayerLynchedMessage = publicMessage [iFile|variant/standard/engine/lynching/no-player-lynched.txt|]

playerLynchedMessage :: Player -> Message
playerLynchedMessage player
    | is simpleWerewolf player
        || is alphaWolf player  = publicMessage [iFile|variant/standard/engine/lynching/werewolf-lynched.txt|]
    | otherwise                 = publicMessage [iFile|variant/standard/engine/lynching/player-lynched.txt|]

scapegoatLynchedMessage :: Player -> Message
scapegoatLynchedMessage scapegoat = publicMessage [iFile|variant/standard/engine/lynching/scapegoat-lynched.txt|]

scapegoatChoseAllowedVotersMessage :: Game -> Message
scapegoatChoseAllowedVotersMessage game = publicMessage [iFile|variant/standard/engine/scapegoats-turn/end.txt|]

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage [iFile|variant/standard/engine/sunrise/no-player-devoured.txt|]

playerDevouredMessage :: Player -> Message
playerDevouredMessage player = publicMessage [iFile|variant/standard/engine/sunrise/player-devoured.txt|]

playerDivinedMessage :: Text -> Player -> Message
playerDivinedMessage to player = privateMessage to [iFile|variant/standard/engine/sunrise/player-divined.txt|]

playerPoisonedMessage :: Player -> Message
playerPoisonedMessage player = publicMessage [iFile|variant/standard/engine/sunrise/player-poisoned.txt|]

playerSeenMessage :: Text -> Player -> Message
playerSeenMessage to player
    | is alphaWolf player   = privateMessage to [iFile|variant/standard/engine/sunrise/alpha-wolf-seen.txt|]
    | is lycan player       = privateMessage to [iFile|variant/standard/engine/sunrise/lycan-seen.txt|]
    | otherwise             = privateMessage to [iFile|variant/standard/engine/sunrise/player-seen.txt|]
    where
        article = if is loner player then "" else "the " :: Text

playerTurnedToStoneMessage :: Player -> Message
playerTurnedToStoneMessage player = publicMessage [iFile|variant/standard/engine/sunrise/player-turned-to-stone.txt|]

villageDrunkJoinedVillageMessage :: Text -> Message
villageDrunkJoinedVillageMessage to = privateMessage to [iFile|variant/standard/engine/village-drunks-turn/player-joined-village.txt|]

villageDrunkJoinedPackMessages :: Text -> Game -> [Message]
villageDrunkJoinedPackMessages to game =
    privateMessage to [iFile|variant/standard/engine/village-drunks-turn/player-joined-werewolves-private.txt|]
    : groupMessages (map (view name) werewolves) [iFile|variant/standard/engine/village-drunks-turn/player-joined-werewolves-group.txt|]
    where
        villageDrunk    = game ^?! players . villageDrunks
        werewolves      = game ^.. players . traverse . alive . filtered (is werewolf) \\ [villageDrunk]
