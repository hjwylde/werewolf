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
    pingPlayerMessage, pingRoleMessage,

    -- * Status messages
    currentStageMessages, rolesInGameMessage, playersInGameMessage, waitingOnMessage,

    -- * Druid's turn messages
    ferinaGruntsMessage,

    -- * Hunter's turn messages
    playerShotMessage,

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
    playerMadeLynchVoteMessage, playerLynchedMessage, noPlayerLynchedMessage,
    jesterLynchedMessage, scapegoatLynchedMessage,

    -- ** Error messages
    playerHasAlreadyVotedMessage,

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
) where

import Control.Arrow
import Control.Lens
import Control.Lens.Extra

import           Data.List.Extra
import           Data.String.Humanise
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Game.Werewolf.Game
import           Game.Werewolf.Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role     hiding (name)
import qualified Game.Werewolf.Role     as Role

newGameMessages :: Game -> [Message]
newGameMessages game = concat
    [ [newPlayersInGameMessage $ players' ^.. names]
    , [rolesInGameMessage Nothing $ players' ^.. roles]
    , map newPlayerMessage players'
    , beholderMessages
    , trueVillagerMessages
    , fallenAngelMessages
    , stageMessages game
    ]
    where
        players'                = game ^. players
        beholderMessages        = case (,) <$> players' ^? beholders <*> players' ^? seers of
            Just (beholder, seer)   -> [beholderMessage (beholder ^. name) (seer ^. name)]
            _                       -> []
        trueVillagerMessages    = case players' ^? trueVillagers of
            Just trueVillager   -> [trueVillagerMessage $ trueVillager ^. name]
            _                   -> []
        fallenAngelMessages     = if has fallenAngels players'
            then [fallenAngelMessage]
            else []

newPlayersInGameMessage :: [Text] -> Message
newPlayersInGameMessage playerNames = publicMessage $ T.concat
    ["A new game of werewolf is starting with ", concatList playerNames, "!"]

newPlayerMessage :: Player -> Message
newPlayerMessage player = privateMessage (player ^. name) $ T.intercalate "\n"
    [ T.concat ["You're ", article playerRole, " ", playerRole ^. Role.name, "."]
    , playerRole ^. description
    , playerRole ^. rules
    ]
    where
        playerRole = player ^. role

beholderMessage :: Text -> Text -> Message
beholderMessage to seerName = privateMessage to $ T.concat
    [ "The Seer has always been held in high regard among the Villagers. Few are as lucky as you to"
    , " know the Seer, ", seerName, ", personally."
    ]

trueVillagerMessage :: Text -> Message
trueVillagerMessage name = publicMessage $ T.unwords
    [ "Unguarded advice is seldom given, for advice is a dangerous gift, even from the wise to the"
    , "wise, and all courses may run ill. Yet as you feel like you need help, I begrudgingly leave"
    , "you with this:", name, "is the True Villager."
    ]

fallenAngelMessage :: Message
fallenAngelMessage = publicMessage $ T.unwords
    [ "Alas, again I regrettably yield advice: an angelic menace walks among you. Do not cast your"
    , "votes lightly, for they will relish in this opportunity to be free from their terrible"
    , "nightmare."
    ]

stageMessages :: Game -> [Message]
stageMessages game = case game ^. stage of
    FerinasGrunt        -> []
    GameOver            -> []
    HuntersTurn1        -> huntersTurnMessages huntersName
    HuntersTurn2        -> huntersTurnMessages huntersName
    Lynching            -> []
    OrphansTurn         -> orphansTurnMessages orphansName
    ProtectorsTurn      -> protectorsTurnMessages protectorsName
    ScapegoatsTurn      -> scapegoatsTurnMessages scapegoatsName
    SeersTurn           -> seersTurnMessages seersName
    Sunrise             -> [sunriseMessage]
    Sunset              -> [nightFallsMessage]
    VillageDrunksTurn   -> [villageDrunksTurnMessage]
    VillagesTurn        -> villagesTurnMessages
    WerewolvesTurn      -> if is firstRound game
        then firstWerewolvesTurnMessages aliveWerewolfNames
        else werewolvesTurnMessages aliveWerewolfNames
    WitchsTurn          -> witchsTurnMessages game
    where
        players'            = game ^. players
        huntersName         = players' ^?! hunters . name
        orphansName         = players' ^?! orphans . name
        protectorsName      = players' ^?! protectors . name
        scapegoatsName      = players' ^?! scapegoats . name
        seersName           = players' ^?! seers . name
        aliveWerewolfNames  = players' ^.. werewolves . alive . name

huntersTurnMessages :: Text -> [Message]
huntersTurnMessages huntersName =
    [ publicMessage $ T.unwords ["Just before", huntersName, "was murdered they let off a shot."]
    , privateMessage huntersName "Whom do you `choose` to kill with your last shot?"
    ]

orphansTurnMessages :: Text -> [Message]
orphansTurnMessages to =
    [ publicMessage "The Orphan wakes up."
    , privateMessage to "Whom do you `choose` to be your role model?"
    ]

protectorsTurnMessages :: Text -> [Message]
protectorsTurnMessages to =
    [ publicMessage "The Protector wakes up."
    , privateMessage to "Whom would you like to `protect`?"
    ]

scapegoatsTurnMessages :: Text -> [Message]
scapegoatsTurnMessages scapegoatsName =
    [ publicMessage "Just before the Scapegoat burns to a complete crisp, they cry out a dying wish."
    , publicMessage $ T.concat [scapegoatsName, ", which players do you `choose` to vote on the next day?"]
    ]

seersTurnMessages :: Text -> [Message]
seersTurnMessages to =
    [ publicMessage "The Seer wakes up."
    , privateMessage to "Whose allegiance would you like to `see`?"
    ]

villageDrunksTurnMessage :: Message
villageDrunksTurnMessage = publicMessage "The Village Drunk sobers up."

sunriseMessage :: Message
sunriseMessage = publicMessage "The sun rises. Everybody wakes up and opens their eyes..."

nightFallsMessage :: Message
nightFallsMessage = publicMessage "Night falls, the village is asleep."

villagesTurnMessages :: [Message]
villagesTurnMessages =
    [ publicMessage "As the village gathers in the square the Town Clerk calls for a vote."
    , publicMessage "Whom would you like to `vote` to lynch?"
    ]

firstWerewolvesTurnMessages :: [Text] -> [Message]
firstWerewolvesTurnMessages tos =
    [privateMessage to $ packMessage to | length tos > 1, to <- tos]
    ++ werewolvesTurnMessages tos
    where
        packMessage werewolfName    = T.unwords
            [ "You feel restless, like an old curse is keeping you from sleep. It seems you're not"
            , "the only one...", packNames werewolfName
            , conjugateToBe (length tos - 1), "also emerging from their"
            , tryPlural (length tos - 1) "home"
            ]
        packNames werewolfName      = concatList $ tos \\ [werewolfName]

werewolvesTurnMessages :: [Text] -> [Message]
werewolvesTurnMessages tos =
    publicMessage "The Werewolves wake up, transform and choose a new victim."
    : groupMessages tos "Whom would you like to `vote` to devour?"

witchsTurnMessages :: Game -> [Message]
witchsTurnMessages game = concat
    [ [wakeUpMessage]
    , devourMessages
    , healMessages
    , poisonMessages
    , [passMessage]
    ]
    where
        witchsName      = game ^?! players . witches . name
        wakeUpMessage   = publicMessage "The Witch wakes up."
        passMessage     = privateMessage witchsName "Type `pass` to end your turn."
        devourMessages  = case game ^? events . traverse . _DevourEvent of
            Just targetName ->
                [ privateMessage witchsName $
                    T.unwords ["You see", targetName, "sprawled outside bleeding uncontrollably."]
                ]
            _               -> []
        healMessages
            | game ^. healUsed                                  = []
            | hasn't (events . traverse . _DevourEvent) game    = []
            | otherwise                                         = [privateMessage witchsName "Would you like to `heal` them?"]
        poisonMessages
            | game ^. poisonUsed    = []
            | otherwise             = [privateMessage witchsName "Would you like to `poison` anyone?"]

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
            , concatList $ map
                (\player -> T.concat [player ^. name, " (", player ^. role . Role.name, ")"])
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
playerWonMessage to = privateMessage to "Victory! You won!"

playerContributedMessage :: Text -> Message
playerContributedMessage to = privateMessage to "Your team won, but you died. Congratulations?"

playerLostMessage :: Text -> Message
playerLostMessage to = privateMessage to "Feck, you lost this time round."

playerQuitMessage :: Player -> Message
playerQuitMessage player = publicMessage $ T.unwords [playerName, "the", playerRole, "has quit!"]
    where
        playerName = player ^. name
        playerRole = player ^. role . Role.name

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to "The game is over!"

playerKilledMessage :: Text -> Message
playerKilledMessage to = privateMessage to "Guh, you've been killed!"

playerDoesNotExistMessage :: Text -> Text -> Message
playerDoesNotExistMessage to name = privateMessage to $ T.unwords ["Player", name, "does not exist."]

playerCannotDoThatMessage :: Text -> Message
playerCannotDoThatMessage to = privateMessage to "You cannot do that!"

playerCannotDoThatRightNowMessage :: Text -> Message
playerCannotDoThatRightNowMessage to = privateMessage to "You cannot do that right now!"

playerIsDeadMessage :: Text -> Message
playerIsDeadMessage to = privateMessage to "Sshh, you're meant to be dead!"

targetIsDeadMessage :: Text -> Text -> Message
targetIsDeadMessage to targetName = privateMessage to $ T.unwords [targetName, "is already dead!"]

playerVotedToBootMessage :: Text -> Text -> Message
playerVotedToBootMessage playerName targetName = publicMessage $ T.concat
    [playerName, " voted to boot ", targetName, "!"]

playerBootedMessage :: Player -> Message
playerBootedMessage player = publicMessage $ T.unwords
    [ playerName, "the", playerRole ^. Role.name
    , "has been booted from the game!"
    ]
    where
        playerName = player ^. name
        playerRole = player ^. role

playerHasAlreadyVotedToBootMessage :: Text -> Text -> Message
playerHasAlreadyVotedToBootMessage to targetName = privateMessage to $ T.concat
    ["You've already voted to boot ", targetName, "!"]

circleMessage :: Text -> [Player] -> Message
circleMessage to players = privateMessage to $ T.intercalate "\n"
    [ "The players are sitting in the following order:"
    , T.intercalate " <-> " (map playerName (players ++ [head players]))
    ]
    where
        playerName player = T.concat [player ^. name, if is dead player then " (dead)" else ""]

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to "Waiting on you..."

pingRoleMessage :: Text -> Message
pingRoleMessage roleName = publicMessage $ T.concat ["Waiting on the ", roleName, "..."]

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
    , concatList $ map (\(role, count) ->
        T.concat [role ^. Role.name, " (", T.pack $ show count, ")"])
        roleCounts
    , " for a total balance of ", T.pack $ show totalBalance, "."
    ]
    where
        roleCounts      = map (head &&& length) (groupSortOn (view Role.name) roles)
        totalBalance    = sumOf (traverse . balance) roles

playersInGameMessage :: Text -> [Player] -> Message
playersInGameMessage to players = privateMessage to . T.intercalate "\n" $
    alivePlayersText : [deadPlayersText | any (is dead) players]
    where
        alivePlayers    = players ^.. traverse . alive
        deadPlayers     = players ^.. traverse . dead

        alivePlayersText            = T.concat
            [ "The following players are still alive: "
            , concatList $ map (\player -> if is trueVillager player then playerNameWithRole player else player ^. name) alivePlayers, "."
            ]
        deadPlayersText             = T.concat
            [ "The following players are dead: "
            , concatList $ map playerNameWithRole deadPlayers, "."
            ]
        playerNameWithRole player   = T.concat [player ^. name, " (", player ^. role . Role.name, ")"]

waitingOnMessage :: Maybe Text -> [Text] -> Message
waitingOnMessage mTo playerNames = Message mTo $ T.concat
    ["Waiting on ", concatList playerNames, "..."]

ferinaGruntsMessage :: Message
ferinaGruntsMessage = publicMessage
    "Ferina wakes from her slumber, disturbed and on edge. She loudly grunts as she smells danger."

playerShotMessage :: Player -> Message
playerShotMessage target = publicMessage $ T.unwords
    [ targetName, "the", targetRole, "slumps down to the ground, hands clutching at their chest"
    , "while blood slips between their fingers and pools around them."
    ]
    where
        targetName = target ^. name
        targetRole = target ^. role . Role.name

orphanJoinedPackMessages :: Text -> [Text] -> [Message]
orphanJoinedPackMessages orphansName werewolfNames =
    privateMessage orphansName (T.unwords
        [ "The death of your role model is distressing. Without second thought you abandon the"
        , "Villagers and run off into the woods, towards a new home. As you arrive you see the"
        , tryPlural (length werewolfNames) "face", "of"
        , concatList werewolfNames, "waiting for you."
        ])
    : groupMessages werewolfNames (T.unwords
        [ orphansName, "the Orphan scampers off into the woods. Without their role model they have"
        , "abandoned the village and are in search of a new home. You welcome them into your pack."
        ])

playerCannotProtectSamePlayerTwiceInARowMessage :: Text -> Message
playerCannotProtectSamePlayerTwiceInARowMessage to =
    privateMessage to "You cannot protect the same player twice in a row!"

scapegoatChoseAllowedVotersMessage :: [Text] -> Message
scapegoatChoseAllowedVotersMessage allowedVoters = publicMessage $ T.unwords
    [ "On the next day only", concatList allowedVoters, "shall be allowed to vote. The Town Crier,"
    , "realising how foolish it was to kill the Scapegoat, grants them this wish."
    ]

playerMustChooseAtLeastOneTargetMessage :: Text -> Message
playerMustChooseAtLeastOneTargetMessage to =
    privateMessage to "You must choose at least 1 target!"

playerCannotChooseJesterMessage :: Text -> Message
playerCannotChooseJesterMessage to =
    privateMessage to "You cannot choose the Jester!"

playerSeenMessage :: Text -> Player -> Message
playerSeenMessage to target = privateMessage to $ T.concat
    [targetName, " is aligned with ", article, humanise allegiance', "."]
    where
        targetName  = target ^. name
        allegiance'
            | is alphaWolf target   = Villagers
            | is lycan target       = Werewolves
            | otherwise             = target ^. role . allegiance
        article     = if allegiance' == NoOne then "" else "the "

villageDrunkJoinedVillageMessage :: Text -> Message
villageDrunkJoinedVillageMessage to = privateMessage to $ T.unwords
    [ "Somehow you managed to avoid getting killed while in your drunken stupor. Thank God for"
    , "that, maybe now you can actually help the village."
    ]

villageDrunkJoinedPackMessages :: Text -> [Text] -> [Message]
villageDrunkJoinedPackMessages villageDrunksName werewolfNames =
    privateMessage villageDrunksName (T.concat
        [ "As you start to feel sober for the first time in days a new thirst begins to take hold."
        , " The bloodthirst starts to bring back memories, memories of your true home with "
        , concatList werewolfNames, "."
        ])
    : groupMessages werewolfNames (T.unwords
        [ villageDrunksName
        , "the Village Drunk has finally sobered up and remembered their true home."
        ])

playerMadeLynchVoteMessage :: Maybe Text -> Text -> Text -> Message
playerMadeLynchVoteMessage mTo voterName targetName = Message mTo $ T.concat
    [ voterName, " voted to lynch ", targetName, "."
    ]

playerLynchedMessage :: Player -> Message
playerLynchedMessage player
    | is simpleWerewolf player
        || is alphaWolf player  = publicMessage $ T.concat
        [ playerName, " is tied up to a pyre and set alight. As they scream their body starts to "
        , "contort and writhe, transforming into ", article playerRole, " "
        , playerRole ^. Role.name, ".", " Thankfully they go limp before breaking free of their "
        , "restraints."
        ]
    | otherwise                 = publicMessage $ T.concat
        [ playerName, " is tied up to a pyre and set alight. Eventually the screams start to die "
        , "and with their last breath, they reveal themselves as ", article playerRole, " "
        , playerRole ^. Role.name, "."
        ]
    where
        playerName = player ^. name
        playerRole = player ^. role

noPlayerLynchedMessage :: Message
noPlayerLynchedMessage = publicMessage $ T.unwords
    [ "Daylight is wasted as the townsfolk squabble over whom to tie up. Looks like no-one is being"
    , "burned this day."
    ]

jesterLynchedMessage :: Text -> Message
jesterLynchedMessage name = publicMessage $ T.concat
    [ "Just as the townsfolk tie ", name, " up to the pyre, a voice in the crowd yells out."
    , " \"We can't burn ", name, "! He's the joke of the town!\" "
    , name, " the Jester is quickly untied and apologised to."
    ]

scapegoatLynchedMessage :: Text -> Message
scapegoatLynchedMessage name = publicMessage $ T.unwords
    [ "The townsfolk squabble over whom to tie up. Just as they are about to call it a day"
    , "they notice that", name, "has been acting awfully suspicious."
    , "Not wanting to take any chances,", name, "is promptly tied to a pyre and burned alive."
    ]

playerHasAlreadyVotedMessage :: Text -> Message
playerHasAlreadyVotedMessage to = privateMessage to "You've already voted!"

playerMadeDevourVoteMessage :: Text -> Text -> Text -> Message
playerMadeDevourVoteMessage to voterName targetName = privateMessage to $ T.concat
    [ voterName, " voted to devour ", targetName, "."
    ]

playerDevouredMessage :: Player -> Message
playerDevouredMessage player = publicMessage $ T.concat
    [ "As you open them you notice a door broken down and "
    , playerName, "'s guts half devoured and spilling out over the cobblestones."
    , " From the look of their personal effects, you deduce they were "
    , article playerRole, " ", playerRole ^. Role.name, "."
    ]
    where
        playerName = player ^. name
        playerRole = player ^. role

playerTurnedToStoneMessage :: Player -> Message
playerTurnedToStoneMessage player = publicMessage $ T.unwords
    [ "Next to them you see a stone", playerRole, "statue, cold to the touch.", playerName
    , "must have looked into the eyes of the Medusa at the very end."
    ]
    where
        playerName = player ^. name
        playerRole = player ^. role . Role.name

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage $ T.unwords
    [ "Surprisingly you see everyone present at the town square."
    , "Perhaps the Werewolves have left Fougères?"
    ]

playerCannotDevourAnotherWerewolfMessage :: Text -> Message
playerCannotDevourAnotherWerewolfMessage to = privateMessage to "You cannot devour another Werewolf!"

playerCannotChooseSelfMessage :: Text -> Message
playerCannotChooseSelfMessage to = privateMessage to "You cannot choose yourself!"

playerPoisonedMessage :: Player -> Message
playerPoisonedMessage player = publicMessage $ T.unwords
    [ "Upon further discovery, it looks like the Witch struck in the night."
    , playerName, "the", playerRole, "is hanging over the side of their bed, poisoned."
    ]
    where
        playerName = player ^. name
        playerRole = player ^. role . Role.name

playerHasAlreadyHealedMessage :: Text -> Message
playerHasAlreadyHealedMessage to = privateMessage to "You've already healed someone!"

playerHasAlreadyPoisonedMessage :: Text -> Message
playerHasAlreadyPoisonedMessage to = privateMessage to "You've already poisoned someone!"

article :: Role -> Text
article role
    | role `elem` restrictedRoles   = "the"
    | otherwise                     = "a"

concatList :: [Text] -> Text
concatList []       = ""
concatList [word]   = word
concatList words    = T.unwords [T.intercalate ", " (init words), "and", last words]

conjugateToBe :: Int -> Text
conjugateToBe 1 = "is"
conjugateToBe _ = "are"

tryPlural :: Int -> Text -> Text
tryPlural 1 word = word
tryPlural _ word = T.snoc word 's'
