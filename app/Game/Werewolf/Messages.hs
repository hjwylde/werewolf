{-|
Module      : Game.Werewolf.Messages
Description : Suite of messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of messages used throughout the werewolf game, including both game play messages and
binary errors.

@werewolf@ was designed to be ambivalent to the playing chat client. The response-message structure
reflects this by staying away from anything that could be construed as client-specific. This
includes features such as emoji support.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Game.Werewolf.Messages (
    -- * Generic messages
    newGameMessages, stageMessages, gameOverMessages, playerQuitMessage, gameIsOverMessage,
    playerKilledMessage,

    -- ** Error messages
    playerDoesNotExistMessage, playerCannotDoThatMessage, playerCannotDoThatRightNowMessage,
    playerIsDeadMessage, targetIsDeadMessage,

    -- * Boot messages
    playerVotedToBootMessage, playerBootedMessage,

    -- ** Error messages
    playerHasAlreadyVotedToBootMessage,

    -- * Circle messages
    circleMessage,

    -- * Ping messages
    pingPlayerMessage, pingRoleMessage, pingVillageMessage, pingWerewolvesMessage,

    -- * Status messages
    currentStageMessages, rolesInGameMessage, playersInGameMessage,

    -- * Druid's turn messages
    ferinaGruntsMessage,

    -- * Hunter's turn messages
    playerShotMessage,

    -- * Oracle's turn messages
    playerDivinedMessage,

    -- * Orphan's turn messages
    orphanJoinedPackMessages,

    -- * Protector's turn messages

    -- ** Error messages
    playerCannotProtectSamePlayerTwiceInARowMessage,

    -- * Scapegoat's turn messages
    scapegoatChoseAllowedVotersMessage,

    -- ** Error messages
    playerMustChooseAtLeastOneTargetMessage, playerCannotChooseJesterMessage,

    -- * Seer's turn messages
    playerSeenMessage,

    -- * Village Drunk's turn messages
    villageDrunkJoinedVillageMessage, villageDrunkJoinedPackMessages,

    -- * Villages' turn messages
    playerMadeLynchVoteMessage, playerRescindedVoteMessage, playerLynchedMessage,
    noPlayerLynchedMessage, jesterLynchedMessage, scapegoatLynchedMessage,

    -- ** Error messages
    playerHasAlreadyVotedMessage, playerHasNotVotedMessage,

    -- * Werewolves' turn messages
    playerMadeDevourVoteMessage, playerDevouredMessage, playerTurnedToStoneMessage,
    noPlayerDevouredMessage,

    -- ** Error messages
    playerCannotDevourAnotherWerewolfMessage,

    -- ** Error messages
    playerCannotChooseSelfMessage,

    -- * Witch's turn messages
    playerPoisonedMessage,

    -- ** Error messages
    playerHasAlreadyHealedMessage, playerHasAlreadyPoisonedMessage,

    -- * Command messages

    -- ** command.version
    engineVersionMessage,

    -- * Error messages

    -- ** error.command
    noGameRunningMessage,

    -- ** error.command.start
    gameAlreadyRunningMessage, roleDoesNotExistMessage,
) where

import Control.Arrow
import Control.Lens       hiding (isn't)
import Control.Lens.Extra

import           Data.List.Extra
import           Data.String.Humanise
import           Data.String.Interpolate.Extra
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Version

import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role     hiding (name)

import Werewolf.Version

newGameMessages :: Game -> [Message]
newGameMessages game = concat
    [ [newPlayersInGameMessage game]
    , [rolesInGameMessage Nothing $ players' ^.. roles]
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
    FerinasGrunt        -> []
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
        then firstWerewolvesTurnMessages aliveWerewolfNames
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

firstWerewolvesTurnMessages :: [Text] -> [Message]
firstWerewolvesTurnMessages tos =
    [privateMessage to $ packMessage to | length tos > 1, to <- tos]
    ++ werewolvesTurnMessages tos
    where
        packMessage werewolfName    = T.unwords
            [ "You feel restless, like an old curse is keeping you from sleep. It seems you're not"
            , "the only one...", packNames werewolfName
            , conjugateToBe (length tos - 1), "also emerging from their"
            , pluralise (length tos - 1) "home"
            ]
        packNames werewolfName      = humanise $ tos \\ [werewolfName]

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

gameOverMessages :: Game -> [Message]
gameOverMessages game
    | hasFallenAngelWon game    = concat
        [ [publicMessage "You should have heeded my warning, for now the Fallen Angel has been set free!"]
        , [publicMessage "The game is over! The Fallen Angel has won."]
        , [playerRolesMessage]
        , [playerWonMessage fallenAngelsName]
        , map playerLostMessage (game ^.. players . names \\ [fallenAngelsName])
        ]
    | hasVillagersWon game      = concat
        [ [publicMessage "The game is over! The Villagers have won."]
        , [playerRolesMessage]
        , playerWonMessages
        , playerContributedMessages
        , playerLostMessages
        ]
    | hasWerewolvesWon game     = concat
        [ [publicMessage "The game is over! The Werewolves have won."]
        , [playerRolesMessage]
        , playerWonMessages
        , playerContributedMessages
        , playerLostMessages
        ]
    | otherwise             = undefined
    where
        playerRolesMessage = publicMessage $ T.concat
            [ "As I know you're all wondering who lied to you, here's the role allocations: "
            , humanise $ map
                (\player -> T.concat [humanise player, " (", humanise $ player ^. role, ")"])
                (game ^. players)
            , "."
            ]

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

playerWonMessage :: Text -> Message
playerWonMessage to = privateMessage to [iFile|messages/engine/game-over/player-won.text|]

playerContributedMessage :: Text -> Message
playerContributedMessage to = privateMessage to [iFile|messages/engine/game-over/player-contributed.text|]

playerLostMessage :: Text -> Message
playerLostMessage to = privateMessage to [iFile|messages/engine/game-over/player-lost.text|]

playerQuitMessage :: Player -> Message
playerQuitMessage caller = publicMessage [iFile|messages/command/quit/player-quit.text|]

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to "The game is over!"

playerKilledMessage :: Text -> Message
playerKilledMessage to = privateMessage to [iFile|messages/engine/general/player-killed.text|]

playerDoesNotExistMessage :: Text -> Text -> Message
playerDoesNotExistMessage to name = privateMessage to [iFile|messages/error/command/general/player-does-not-exist.text|]

playerCannotDoThatMessage :: Text -> Message
playerCannotDoThatMessage to = privateMessage to [iFile|messages/error/command/general/player-cannot-do-that.text|]

playerCannotDoThatRightNowMessage :: Text -> Message
playerCannotDoThatRightNowMessage to = privateMessage to [iFile|messages/error/command/general/player-cannot-do-that-right-now.text|]

playerIsDeadMessage :: Text -> Message
playerIsDeadMessage to = privateMessage to [iFile|messages/error/command/general/player-dead.text|]

targetIsDeadMessage :: Text -> Player -> Message
targetIsDeadMessage to target = privateMessage to [iFile|messages/error/command/general/target-dead.text|]

playerVotedToBootMessage :: Player -> Player -> Message
playerVotedToBootMessage caller target = publicMessage [iFile|messages/command/boot/player-voted-boot.text|]

playerBootedMessage :: Player -> Message
playerBootedMessage player = publicMessage [iFile|messages/command/boot/player-booted.text|]

playerHasAlreadyVotedToBootMessage :: Text -> Player -> Message
playerHasAlreadyVotedToBootMessage to target = privateMessage to [iFile|messages/error/command/boot/player-already-voted-boot.text|]

circleMessage :: Text -> [Player] -> Message
circleMessage to players = privateMessage to $ T.intercalate "\n"
    [ "The players are sitting in the following order:"
    , T.intercalate " <-> " (map playerName (players ++ [head players]))
    ]
    where
        playerName player = T.concat [humanise player, if is dead player then " (dead)" else ""]

currentStageMessages :: Text -> Stage -> [Message]
currentStageMessages _ FerinasGrunt = []
currentStageMessages to GameOver    = [gameIsOverMessage to]
currentStageMessages _ Lynching     = []
currentStageMessages _ Sunrise      = []
currentStageMessages _ Sunset       = []
currentStageMessages to turn        = [privateMessage to $ T.concat
    [ "It's currently the ", humanise turn, "."
    ]]

rolesInGameMessage :: Maybe Text -> [Role] -> Message
rolesInGameMessage mTo roles = Message mTo $ T.concat
    [ "The roles in play are "
    , humanise $ map (\(role, count) ->
        T.concat [humanise role, " (", T.pack $ show count, ")"])
        roleCounts
    , " for a total balance of ", T.pack $ show totalBalance, "."
    ]
    where
        roleCounts      = map (head &&& length) (groupSortOn (humanise :: Role -> Text) roles)
        totalBalance    = sumOf (traverse . balance) roles

playersInGameMessage :: Text -> [Player] -> Message
playersInGameMessage to players = privateMessage to . T.intercalate "\n" $
    alivePlayersText : [deadPlayersText | any (is dead) players]
    where
        alivePlayers    = players ^.. traverse . alive
        deadPlayers     = players ^.. traverse . dead

        alivePlayersText            = T.concat
            [ "The following players are still alive: "
            , humanise $ map (\player -> if is trueVillager player then humanisePlayerWithRole player else humanise player) alivePlayers, "."
            ]
        deadPlayersText             = T.concat
            [ "The following players are dead: "
            , humanisePlayersWithRoles deadPlayers, "."
            ]

orphanJoinedPackMessages :: Text -> Game -> [Message]
orphanJoinedPackMessages to game =
    privateMessage to [iFile|messages/engine/orphans-turn/player-joined-werewolves-private.text|]
    : groupMessages (map (view name) werewolves) [iFile|messages/engine/orphans-turn/player-joined-werewolves-group.text|]
    where
        orphan      = game ^?! players . villageDrunks
        werewolves  = game ^.. players . traverse . alive . filtered (is werewolf) \\ [orphan]

-- TODO (hjw): ordered from here

playerShotMessage :: Player -> Message
playerShotMessage player = publicMessage [iFile|messages/command/choose/player-shot.text|]

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to [iFile|messages/command/ping/player-pinged.text|]

pingRoleMessage :: Role -> Message
pingRoleMessage role = publicMessage [iFile|messages/command/ping/role-pinged.text|]

pingVillageMessage :: Message
pingVillageMessage = publicMessage [iFile|messages/command/ping/village-pinged.text|]

pingWerewolvesMessage :: Message
pingWerewolvesMessage = publicMessage [iFile|messages/command/ping/werewolves-pinged.text|]

playerRescindedVoteMessage :: Text -> Text -> Message
playerRescindedVoteMessage to caller = privateMessage to [iFile|messages/command/unvote/player-rescinded-vote.text|]

engineVersionMessage :: Text -> Message
engineVersionMessage to = privateMessage to [iFile|messages/command/version/engine-version.text|]

playerMadeDevourVoteMessage :: Text -> Player -> Player -> Message
playerMadeDevourVoteMessage to caller target = privateMessage to [iFile|messages/command/vote/player-made-devour-vote.text|]

playerMadeLynchVoteMessage :: Maybe Text -> Text -> Text -> Message
playerMadeLynchVoteMessage mTo caller target = Message mTo [iFile|messages/command/vote/player-made-lynch-vote.text|]

ferinaGruntsMessage :: Message
ferinaGruntsMessage = publicMessage [iFile|messages/engine/ferina-grunts/start.text|]

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

playerCannotChooseSelfMessage :: Text -> Message
playerCannotChooseSelfMessage to = privateMessage to [iFile|messages/error/command/choose/player-cannot-choose-self.text|]

playerCannotChooseJesterMessage :: Text -> Message
playerCannotChooseJesterMessage to = privateMessage to [iFile|messages/error/command/choose/player-cannot-choose-jester.text|]

playerMustChooseAtLeastOneTargetMessage :: Text -> Message
playerMustChooseAtLeastOneTargetMessage to = privateMessage to [iFile|messages/error/command/choose/player-must-choose-at-least-one-target.text|]

noGameRunningMessage :: Text -> Message
noGameRunningMessage to = privateMessage to [iFile|messages/error/command/general/no-game-running.text|]

playerHasAlreadyHealedMessage :: Text -> Message
playerHasAlreadyHealedMessage to = privateMessage to [iFile|messages/error/command/heal/player-already-healed.text|]

playerHasAlreadyPoisonedMessage :: Text -> Message
playerHasAlreadyPoisonedMessage to = privateMessage to [iFile|messages/error/command/poison/player-already-poisoned.text|]

playerCannotProtectSamePlayerTwiceInARowMessage :: Text -> Message
playerCannotProtectSamePlayerTwiceInARowMessage to = privateMessage to [iFile|messages/error/command/protect/player-cannot-protect-same-player-twice-in-a-row.text|]

gameAlreadyRunningMessage :: Text -> Message
gameAlreadyRunningMessage to = privateMessage to [iFile|messages/error/command/start/game-already-running.text|]

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to role = privateMessage to [iFile|messages/error/command/start/role-does-not-exist.text|]

playerHasNotVotedMessage :: Text -> Message
playerHasNotVotedMessage to = privateMessage to [iFile|messages/error/command/unvote/player-not-voted.text|]

playerHasAlreadyVotedMessage :: Text -> Message
playerHasAlreadyVotedMessage to = privateMessage to [iFile|messages/error/command/vote/player-already-voted.text|]

playerCannotDevourAnotherWerewolfMessage :: Text -> Message
playerCannotDevourAnotherWerewolfMessage to = privateMessage to [iFile|messages/error/command/vote/player-cannot-devour-werewolf.text|]

humanisePlayerWithRole :: Player -> Text
humanisePlayerWithRole player = T.concat [humanise player, " (", humanise $ player ^. role, ")"]

humanisePlayersWithRoles :: [Player] -> Text
humanisePlayersWithRoles = humanise . map humanisePlayerWithRole

article :: Role -> Text
article role
    | role `elem` restrictedRoles   = "the"
    | otherwise                     = "a"

conjugateToBe :: Int -> Text
conjugateToBe 1 = "is"
conjugateToBe _ = "are"

pluralise :: Int -> Text -> Text
pluralise 1 word = word
pluralise _ word = T.snoc word 's'
