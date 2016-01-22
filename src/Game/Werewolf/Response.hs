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
    exitWith, exitSuccess, exitFailure,

    -- * Message
    Message(..),
    publicMessage, privateMessage,

    -- ** Generic messages
    newGameMessages, turnMessages, nightFallsMessage, gameOverMessage, playerQuitMessage,

    -- ** Ping messages
    pingSeersMessage, pingWerewolvesMessage,

    -- ** Status messages
    currentTurnMessage, rolesInGameMessage, playersInGameMessage, waitingOnMessage,

    -- ** Seers turn messages
    seersTurnMessages, playerSeenMessage,

    -- ** Villagers turn messages
    villagersTurnMessage, playerMadeLynchVoteMessage, playerLynchedMessage, noPlayerLynchedMessage,
    scapegoatLynchedMessage,

    -- ** Werewolves turn messages
    werewolvesTurnMessages, playerMadeDevourVoteMessage, playerDevouredMessage,
    noPlayerDevouredMessage, villagersLynchVoteMessage,

    -- ** Generic error messages
    gameIsOverMessage, playerDoesNotExistMessage, playerCannotDoThatMessage,
    playerCannotDoThatRightNowMessage, playerIsDeadMessage, roleDoesNotExistMessage,

    -- ** Seers turn error messages
    playerHasAlreadySeenMessage,

    -- ** Voting turn error messages
    playerHasAlreadyVotedMessage, targetIsDeadMessage,

    -- ** Werewolves turn error messages
    playerCannotDevourAnotherWerewolf,
) where

import Control.Lens
import Control.Monad.IO.Class

import Data.Aeson
#if !MIN_VERSION_aeson(0,10,0)
import Data.Aeson.Types
#endif
import           Data.List.Extra
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO       as T

import           Game.Werewolf.Game
import           Game.Werewolf.Player
import           Game.Werewolf.Role   (Role, allegiance, description)
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

exitSuccess :: MonadIO m => m ()
exitSuccess = exitWith success

exitFailure :: MonadIO m => m ()
exitFailure = exitWith failure

data Message = Message
    { to      :: Maybe [Text]
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

privateMessage :: [Text] -> Text -> Message
privateMessage to = Message (Just to)

newGameMessages :: Game -> [Message]
newGameMessages game = [newPlayersInGameMessage players', rolesInGameMessage Nothing $ map _role players'] ++ map (newPlayerMessage players') players' ++ [nightFallsMessage] ++ turnMessages turn' players'
    where
        turn'       = game ^. turn
        players'    = game ^. players

newPlayersInGameMessage :: [Player] -> Message
newPlayersInGameMessage players = publicMessage $ T.concat [
    "A new game of werewolf is starting with ",
    T.intercalate ", " (map _name players), "!"
    ]

newPlayerMessage :: [Player] -> Player -> Message
newPlayerMessage players player
    | isWerewolf player = privateMessage [player ^. name] $ T.intercalate "\n" [T.concat ["You're a Werewolf", packMessage], player ^. role . description]
    | otherwise         = privateMessage [player ^. name] $ T.intercalate "\n" [T.concat ["You're a ", player ^. role . Role.name, "."], player ^. role . description]
    where
        packMessage
            | length (filterWerewolves players) <= 1    = "."
            | otherwise                                 = T.concat [", along with ", T.intercalate ", " (map _name $ filterWerewolves players \\ [player]), "."]

turnMessages :: Turn -> [Player] -> [Message]
turnMessages Seers players      = seersTurnMessages $ filter isSeer players
turnMessages Villagers _        = [villagersTurnMessage]
turnMessages Werewolves players = werewolvesTurnMessages $ filter isWerewolf players
turnMessages NoOne _            = []

nightFallsMessage :: Message
nightFallsMessage = publicMessage "Night falls, the townsfolk are asleep."

gameOverMessage :: Maybe Text -> Message
gameOverMessage Nothing             = publicMessage "The game is over! Everyone died..."
gameOverMessage (Just allegiance)   = publicMessage $ T.unwords ["The game is over! The", allegiance, "have won."]

playerQuitMessage :: Player -> Message
playerQuitMessage player = publicMessage $ T.unwords [player ^. name, "the", player ^. role . Role.name, "has quit!"]

pingSeersMessage :: Message
pingSeersMessage = publicMessage "Waiting on the Seers..."

pingWerewolvesMessage :: Message
pingWerewolvesMessage = publicMessage "Waiting on the Werewolves..."

currentTurnMessage :: Text -> Turn -> Message
currentTurnMessage name NoOne   = gameIsOverMessage name
currentTurnMessage name turn    = privateMessage [name] $ T.concat [
    "It's currently the ", T.pack $ show turn, "' turn."
    ]

rolesInGameMessage :: Maybe [Text] -> [Role] -> Message
rolesInGameMessage mTo roles = Message mTo $ T.concat [
    "The roles in play are ",
    T.intercalate ", " $ map (\(role, count) ->
        T.concat [role ^. Role.name, " (", T.pack $ show count, ")"])
        roleCounts,
    "."
    ]
    where
        roleCounts = map (\list -> (head list, length list)) (groupSortOn Role._name roles)

playersInGameMessage :: Text -> [Player] -> Message
playersInGameMessage to players = privateMessage [to] . T.intercalate "\n" $ [
    alivePlayersText
    ] ++ if (null $ filterDead players) then [] else [deadPlayersText]
    where
        alivePlayersText = T.concat [
            "The following players are still alive: ",
            T.intercalate ", " (map _name $ filterAlive players), "."
            ]
        deadPlayersText = T.concat [
            "The following players are dead: ",
            T.intercalate ", " (map (\player -> T.concat [player ^. name, " (", player ^. role . Role.name, ")"]) $ filterDead players), "."
            ]

waitingOnMessage :: Maybe [Text] -> [Player] -> Message
waitingOnMessage to players = Message to $ T.concat [
    "Waiting on ", T.intercalate ", " playerNames, "..."
    ]
    where
        playerNames = map _name players

seersTurnMessages :: [Player] -> [Message]
seersTurnMessages seers = [
    publicMessage "The Seers wake up.",
    privateMessage (map _name seers) "Whose allegiance would you like to see?"
    ]

playerSeenMessage :: Text -> Player -> Message
playerSeenMessage seerName target = privateMessage [seerName] $ T.concat [target ^. name, " is aligned with the ", T.pack . show $ target ^. role . allegiance, "."]

villagersTurnMessage :: Message
villagersTurnMessage = publicMessage "The sun rises. Everybody wakes up and opens their eyes..."

villagersLynchVoteMessage :: Message
villagersLynchVoteMessage = publicMessage "Whom would you like to lynch?"

playerMadeLynchVoteMessage :: Text -> Text -> Message
playerMadeLynchVoteMessage voterName targetName = publicMessage $ T.concat [voterName, " voted to lynch ", targetName, "."]

playerLynchedMessage :: Text -> Text -> Message
playerLynchedMessage name "Werewolf"    = publicMessage $ T.unwords [
    name, "is tied up to a pyre and set alight.",
    "As they scream their body starts to contort and writhe, transforming into a Werewolf.",
    "Thankfully they go limp before breaking free of their restraints."
    ]
playerLynchedMessage name roleName      = publicMessage $ T.concat [
    name, " is tied up to a pyre and set alight.",
    " Eventually the screams start to die and with their last breath,",
    " they reveal themselves as a ", roleName, "."
    ]

noPlayerLynchedMessage :: Message
noPlayerLynchedMessage = publicMessage "Daylight is wasted as the townsfolk squabble over whom to tie up. Looks like no one is being burned this day."

werewolvesTurnMessages :: [Player] -> [Message]
werewolvesTurnMessages werewolves = [
    publicMessage "The Werewolves wake up, recognise one another and choose a new victim.",
    privateMessage (map _name werewolves) "Whom would you like to devour?"
    ]

scapegoatLynchedMessage :: Text -> Message
scapegoatLynchedMessage name = publicMessage $ T.unwords [
    "The townsfolk squabble over whom to tie up. Just as they are about to call it a day",
    "they notice that", name, "has been acting awfully suspicious.",
    "Not wanting to take any chances,", name, "is promptly tied to a pyre and burned alive."
    ]

playerMadeDevourVoteMessage :: [Text] -> Text -> Text -> Message
playerMadeDevourVoteMessage to voterName targetName = privateMessage to $ T.concat [voterName, " voted to devour ", targetName, "."]

playerDevouredMessage :: Text -> Text -> Message
playerDevouredMessage name roleName = publicMessage $ T.concat [
    "As you open them you notice a door broken down and ",
    name, "'s guts half devoured and spilling out over the cobblestones.",
    " From the look of their personal effects, you deduce they were a ", roleName, ".",
    " As the village bays for vengeance, the town clerk calls for a vote."
    ]

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage $ T.unwords [
    "Surprisingly you see everyone present at the town square. Perhaps the Werewolves have left Miller's Hollow?",
    "Still got to keep up the tradition though, so the town clerk calls for a vote."
    ]

gameIsOverMessage :: Text -> Message
gameIsOverMessage name = privateMessage [name] "The game is over!"

playerDoesNotExistMessage :: Text -> Text -> Message
playerDoesNotExistMessage to name = privateMessage [to] $ T.unwords ["Player", name, "does not exist."]

playerCannotDoThatMessage :: Text -> Message
playerCannotDoThatMessage name = privateMessage [name] "You cannot do that!"

playerCannotDoThatRightNowMessage :: Text -> Message
playerCannotDoThatRightNowMessage name = privateMessage [name] "You cannot do that right now!"

playerIsDeadMessage :: Text -> Message
playerIsDeadMessage name = privateMessage [name] "Sshh, you're meant to be dead!"

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to name = privateMessage [to] $ T.unwords ["Role", name, "does not exist."]

playerHasAlreadySeenMessage :: Text -> Message
playerHasAlreadySeenMessage name = privateMessage [name] "You've already seen!"

playerHasAlreadyVotedMessage :: Text -> Message
playerHasAlreadyVotedMessage name = privateMessage [name] "You've already voted!"

targetIsDeadMessage :: Text -> Text -> Message
targetIsDeadMessage name targetName = privateMessage [name] $ T.unwords [targetName, "is already dead!"]

playerCannotDevourAnotherWerewolf :: Text -> Message
playerCannotDevourAnotherWerewolf name = privateMessage [name] "You cannot devour another Werewolf!"
