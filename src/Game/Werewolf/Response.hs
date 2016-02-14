{-|
Module      : Game.Werewolf.Response
Description : Response and message data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Response and message data structures.
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Response (
    -- * Response
    Response(..),

    -- ** Common responses
    success, failure,

    -- ** Exit functions
    exitWith,

    -- * Message
    Message(..),
    publicMessage, privateMessage, groupMessages,

    -- ** Binary messages
    noGameRunningMessage, gameAlreadyRunningMessage,

    -- ** Generic messages
    newGameMessages, stageMessages, gameOverMessages, playerQuitMessage,

    -- ** Ping messages
    pingPlayerMessage, pingRoleMessage,

    -- ** Status messages
    currentStageMessages, rolesInGameMessage, playersInGameMessage, waitingOnMessage,

    -- ** Seer's turn messages
    playerSeenMessage,

    -- ** Villages' turn messages
    playerMadeLynchVoteMessage, playerLynchedMessage, noPlayerLynchedMessage,
    scapegoatLynchedMessage,

    -- ** Werewolves' turn messages
    playerMadeDevourVoteMessage, playerDevouredMessage, noPlayerDevouredMessage,

    -- ** Witch's turn messages
    playerPoisonedMessage,

    -- ** Generic error messages
    gameIsOverMessage, playerDoesNotExistMessage, playerCannotDoThatMessage,
    playerCannotDoThatRightNowMessage, playerIsDeadMessage, roleDoesNotExistMessage,
    allegianceDoesNotExistMessage,

    -- ** Seer's turn error messages
    playerCannotProtectSelfMessage, playerCannotProtectSamePlayerTwiceInARowMessage,

    -- ** Voting turn error messages
    playerHasAlreadyVotedMessage, targetIsDeadMessage,

    -- ** Werewolves' turn error messages
    playerCannotDevourAnotherWerewolfMessage,

    -- ** Witch's turn error messages
    playerHasAlreadyHealedMessage, playerHasAlreadyPoisonedMessage,
) where

import Control.Lens
import Control.Monad.IO.Class

import Data.Aeson
#if !MIN_VERSION_aeson(0,10,0)
import Data.Aeson.Types
#endif
import           Data.List.Extra
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO       as T

import           Game.Werewolf.Game
import           Game.Werewolf.Player
import           Game.Werewolf.Role   (Role, allegiance, description, restrictedRoles)
import qualified Game.Werewolf.Role   as Role
import           GHC.Generics

import qualified System.Exit as Exit

data Response = Response
    { ok       :: Bool
    , messages :: [Message]
    } deriving (Eq, Generic, Show)

instance FromJSON Response

instance ToJSON Response where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

success :: Response
success = Response True []

failure :: Response
failure = Response False []

exitWith :: MonadIO m => Response -> m ()
exitWith response = liftIO $ T.putStrLn (T.decodeUtf8 $ encode response) >> Exit.exitSuccess

data Message = Message
    { to      :: Maybe Text
    , message :: Text
    } deriving (Eq, Generic, Show)

instance FromJSON Message

instance ToJSON Message where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

publicMessage :: Text -> Message
publicMessage = Message Nothing

privateMessage :: Text -> Text -> Message
privateMessage to = Message (Just to)

groupMessages :: [Text] -> Text -> [Message]
groupMessages tos message = map (\to -> privateMessage to message) tos

noGameRunningMessage :: Text -> Message
noGameRunningMessage to = privateMessage to "No game is running."

gameAlreadyRunningMessage :: Text -> Message
gameAlreadyRunningMessage to = privateMessage to "A game is already running."

newGameMessages :: Game -> [Message]
newGameMessages game = concat
    [ [newPlayersInGameMessage players', rolesInGameMessage Nothing $ map (view role) players']
    , map newPlayerMessage players'
    , villagerVillagerMessages
    , stageMessages game
    ]
    where
        players'                    = game ^. players
        villagerVillagerMessages    =
            case filterVillagerVillagers players' of
                [villagerVillager]  -> [villagerVillagerMessage $ villagerVillager ^. name]
                _                   -> []

newPlayersInGameMessage :: [Player] -> Message
newPlayersInGameMessage players = publicMessage $ T.concat
    [ "A new game of werewolf is starting with "
    , T.intercalate ", " playerNames, "!"
    ]
    where
        playerNames = map (view name) players

newPlayerMessage :: Player -> Message
newPlayerMessage player = privateMessage (player ^. name) $ T.intercalate "\n"
    [ T.concat ["You're ", article playerRole, " ", playerRole ^. Role.name, "."]
    , playerRole ^. description
    ]
    where
        playerRole = player ^. role

villagerVillagerMessage :: Text -> Message
villagerVillagerMessage name = publicMessage $ T.unwords
    [ "Unguarded advice is seldom given, for advice is a dangerous gift,"
    , "even from the wise to the wise, and all courses may run ill."
    , "Yet as you feel like you need help, I begrudgingly leave you with this:"
    , name, "is the Villager-Villager."
    ]

stageMessages :: Game -> [Message]
stageMessages game = case game ^. stage of
    GameOver        -> []
    DefendersTurn   -> defendersTurnMessages defendersName
    SeersTurn       -> seersTurnMessages seersName
    Sunrise         -> [sunriseMessage]
    Sunset          -> [nightFallsMessage]
    VillagesTurn    -> villagesTurnMessages
    WerewolvesTurn  -> if isFirstRound game
        then firstWerewolvesTurnMessages aliveWerewolfNames
        else werewolvesTurnMessages aliveWerewolfNames
    WitchsTurn      -> witchsTurnMessages game
    WolfHoundsTurn  -> wolfHoundsTurnMessages wolfHoundsName
    where
        players'            = game ^. players
        defendersName       = view name . head $ filterDefenders players'
        seersName           = view name . head $ filterSeers players'
        aliveWerewolfNames  = map (view name) . filterAlive $ filterAlignedWithWerewolves players'
        wolfHoundsName      = view name . head $ filterWolfHounds players'

defendersTurnMessages :: Text -> [Message]
defendersTurnMessages to =
    [ publicMessage "The Defender wakes up."
    , privateMessage to "Whom would you like to protect?"
    ]

seersTurnMessages :: Text -> [Message]
seersTurnMessages to =
    [ publicMessage "The Seer wakes up."
    , privateMessage to "Whose allegiance would you like to see?"
    ]

sunriseMessage :: Message
sunriseMessage = publicMessage "The sun rises. Everybody wakes up and opens their eyes..."

nightFallsMessage :: Message
nightFallsMessage = publicMessage "Night falls, the village is asleep."

villagesTurnMessages :: [Message]
villagesTurnMessages =
    [ publicMessage "As the village gathers in the square the town clerk calls for a vote."
    , publicMessage "Whom would you like to lynch?"
    ]

firstWerewolvesTurnMessages :: [Text] -> [Message]
firstWerewolvesTurnMessages tos = concat
    [ map (\to -> privateMessage to $ packMessage to) tos
    , werewolvesTurnMessages tos
    ]
    where
        packMessage werewolfName    = T.unwords
            [ "You feel restless, like an old curse is keeping you from sleep."
            , "It seems you're not the only one..."
            , packNames werewolfName
            , "are also emerging from their homes."
            ]
        packNames werewolfName      = T.intercalate ", " (tos \\ [werewolfName])

werewolvesTurnMessages :: [Text] -> [Message]
werewolvesTurnMessages tos =
    [ publicMessage "The Werewolves wake up, recognise one another and choose a new victim."
    ] ++ groupMessages tos "Whom would you like to devour?"

witchsTurnMessages :: Game -> [Message]
witchsTurnMessages game = concat
    [ [wakeUpMessage]
    , devourMessages
    , healMessages
    , poisonMessages
    , [passMessage]
    ]
    where
        witchsName      = (head . filterWitches $ game ^. players) ^. name
        wakeUpMessage   = publicMessage "The Witch wakes up."
        passMessage     = privateMessage witchsName "Type `pass` to end your turn."
        devourMessages  = case getDevourEvent game of
            Just (DevourEvent targetName)   ->
                [ privateMessage witchsName $
                    T.unwords ["You see", targetName, "sprawled outside bleeding uncontrollably."]
                ]
            _                               -> []
        healMessages
            | not (game ^. healUsed)
                && isJust (getDevourEvent game) = [privateMessage witchsName "Would you like to heal them?"]
            | otherwise                         = []
        poisonMessages
            | not (game ^. poisonUsed)          = [privateMessage witchsName "Would you like to poison anyone?"]
            | otherwise                         = []

wolfHoundsTurnMessages :: Text -> [Message]
wolfHoundsTurnMessages to =
    [ publicMessage "The Wolf-hound wakes up."
    , privateMessage to "Whom would you like to be aligned with?"
    ]

gameOverMessages :: Game -> [Message]
gameOverMessages game = case aliveAllegiances of
    [allegiance']    -> concat
        [ [publicMessage $ T.unwords ["The game is over! The", T.pack $ show allegiance', "have won."]]
        , map (playerWonMessage . view name) (filter ((allegiance' ==) . view (role . allegiance)) players')
        , map (playerLostMessage . view name) (filter ((allegiance' /=) . view (role . allegiance)) players')
        ]
    _               -> publicMessage "The game is over! Everyone died...":map (playerLostMessage . view name) players'
    where
        players'            = game ^. players
        aliveAllegiances    = nub $ map (view $ role . allegiance) (filterAlive players')

playerWonMessage :: Text -> Message
playerWonMessage to = privateMessage to "Victory! You won!"

playerLostMessage :: Text -> Message
playerLostMessage to = privateMessage to "Feck, you lost this time round..."

playerQuitMessage :: Player -> Message
playerQuitMessage player = publicMessage $ T.unwords [player ^. name, "the", player ^. role . Role.name, "has quit!"]

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to "Waiting on you..."

pingRoleMessage :: Text -> Message
pingRoleMessage roleName = publicMessage $ T.concat ["Waiting on the ", roleName, "..."]

currentStageMessages :: Text -> Stage -> [Message]
currentStageMessages to GameOver    = [gameIsOverMessage to]
currentStageMessages _ Sunrise      = []
currentStageMessages _ Sunset       = []
    -- TODO (hjw): pluralise this correctly for the Seer
currentStageMessages to turn        = [privateMessage to $ T.concat
    [ "It's currently the ", T.pack $ show turn, "' turn."
    ]]

rolesInGameMessage :: Maybe Text -> [Role] -> Message
rolesInGameMessage mTo roles = Message mTo $ T.concat
    [ "The roles in play are "
    , T.intercalate ", " $ map (\(role, count) ->
        T.concat [role ^. Role.name, " (", T.pack $ show count, ")"])
        roleCounts
    , "."
    ]
    where
        roleCounts = map (\list -> (head list, length list)) (groupSortOn (view Role.name) roles)

playersInGameMessage :: Text -> [Player] -> Message
playersInGameMessage to players = privateMessage to . T.intercalate "\n" $
    [ alivePlayersText
    ] ++ if (null $ filterDead players) then [] else [deadPlayersText]
    where
        alivePlayersText = T.concat
            [ "The following players are still alive: "
            , T.intercalate ", " (map (view name) $ filterAlive players), "."
            ]
        deadPlayersText = T.concat
            [ "The following players are dead: "
            , T.intercalate ", " (map (\player -> T.concat [player ^. name, " (", player ^. role . Role.name, ")"]) $ filterDead players), "."
            ]

waitingOnMessage :: Maybe Text -> [Player] -> Message
waitingOnMessage mTo players = Message mTo $ T.concat
    [ "Waiting on ", T.intercalate ", " playerNames, "..."
    ]
    where
        playerNames = map (view name) players

playerSeenMessage :: Text -> Player -> Message
playerSeenMessage to target = privateMessage to $ T.concat
    [ targetName, " is aligned with the ", T.pack $ show allegiance', "."
    ]
    where
        targetName  = target ^. name
        allegiance' = target ^. role . allegiance

playerMadeLynchVoteMessage :: Text -> Text -> Message
playerMadeLynchVoteMessage voterName targetName = publicMessage $ T.concat
    [ voterName, " voted to lynch ", targetName, "."
    ]

playerLynchedMessage :: Player -> Message
playerLynchedMessage player
    | isAlignedWithWerewolves player = publicMessage $ T.unwords
        [ player ^. name, "is tied up to a pyre and set alight."
        , "As they scream their body starts to contort and writhe, transforming into a Werewolf."
        , "Thankfully they go limp before breaking free of their restraints."
        ]
    | otherwise         = publicMessage $ T.concat
        [ player ^. name, " is tied up to a pyre and set alight."
        , " Eventually the screams start to die and with their last breath,"
        , " they reveal themselves as "
        , article $ player ^. role, " ", player ^. role . Role.name, "."
        ]

noPlayerLynchedMessage :: Message
noPlayerLynchedMessage = publicMessage $ T.unwords
    [ "Daylight is wasted as the townsfolk squabble over whom to tie up."
    , "Looks like no one is being burned this day."
    ]

scapegoatLynchedMessage :: Text -> Message
scapegoatLynchedMessage name = publicMessage $ T.unwords
    [ "The townsfolk squabble over whom to tie up. Just as they are about to call it a day"
    , "they notice that", name, "has been acting awfully suspicious."
    , "Not wanting to take any chances,", name, "is promptly tied to a pyre and burned alive."
    ]

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

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage $ T.unwords
    [ "Surprisingly you see everyone present at the town square."
    , "Perhaps the Werewolves have left Miller's Hollow?"
    ]

playerPoisonedMessage :: Player -> Message
playerPoisonedMessage player = publicMessage $ T.unwords
    [ "Upon further discovery, it looks like the Witch struck in the night."
    , player ^. name, "the", player ^. role . Role.name
    , "is lying in their bed, poisoned, drooling over the side."
    ]

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to "The game is over!"

playerDoesNotExistMessage :: Text -> Text -> Message
playerDoesNotExistMessage to name = privateMessage to $ T.unwords
    [ "Player", name, "does not exist."
    ]

playerCannotDoThatMessage :: Text -> Message
playerCannotDoThatMessage to = privateMessage to "You cannot do that!"

playerCannotDoThatRightNowMessage :: Text -> Message
playerCannotDoThatRightNowMessage to = privateMessage to "You cannot do that right now!"

playerIsDeadMessage :: Text -> Message
playerIsDeadMessage to = privateMessage to "Sshh, you're meant to be dead!"

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to name = privateMessage to $ T.unwords ["Role", name, "does not exist."]

allegianceDoesNotExistMessage :: Text -> Text -> Message
allegianceDoesNotExistMessage to name = privateMessage to $ T.unwords
    [ "Allegiance", name, "does not exist."
    ]

playerCannotProtectSelfMessage :: Text -> Message
playerCannotProtectSelfMessage to = privateMessage to "You cannot protect yourself!"

playerCannotProtectSamePlayerTwiceInARowMessage :: Text -> Message
playerCannotProtectSamePlayerTwiceInARowMessage to =
    privateMessage to "You cannot protect the same player twice in a row!"

playerHasAlreadyVotedMessage :: Text -> Message
playerHasAlreadyVotedMessage to = privateMessage to "You've already voted!"

targetIsDeadMessage :: Text -> Text -> Message
targetIsDeadMessage to targetName = privateMessage to $ T.unwords
    [ targetName, "is already dead!"
    ]

playerCannotDevourAnotherWerewolfMessage :: Text -> Message
playerCannotDevourAnotherWerewolfMessage to =
    privateMessage to "You cannot devour another Werewolf!"

playerHasAlreadyHealedMessage :: Text -> Message
playerHasAlreadyHealedMessage to = privateMessage to "You've already healed someone!"

playerHasAlreadyPoisonedMessage :: Text -> Message
playerHasAlreadyPoisonedMessage to = privateMessage to "You've already poisoned someone!"

article :: Role -> Text
article role
    | role `elem` restrictedRoles   = "the"
    | otherwise                     = "a"
