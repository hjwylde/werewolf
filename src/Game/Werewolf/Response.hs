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
    pingPlayerMessage, pingSeerMessage, pingWerewolvesMessage,

    -- ** Status messages
    currentStageMessages, rolesInGameMessage, playersInGameMessage, waitingOnMessage,

    -- ** Seer's turn messages
    playerSeenMessage,

    -- ** Villages' turn messages
    playerMadeLynchVoteMessage, playerLynchedMessage, noPlayerLynchedMessage,
    scapegoatLynchedMessage,

    -- ** Werewolves' turn messages
    playerMadeDevourVoteMessage, playerDevouredMessage, noPlayerDevouredMessage,

    -- ** Generic error messages
    gameIsOverMessage, playerDoesNotExistMessage, playerCannotDoThatMessage,
    playerCannotDoThatRightNowMessage, playerIsDeadMessage, roleDoesNotExistMessage,

    -- ** Voting turn error messages
    playerHasAlreadyVotedMessage, targetIsDeadMessage,

    -- ** Werewolves' turn error messages
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
import           Game.Werewolf.Role   (Role, allegiance, description, _allegiance)
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
newGameMessages game = [
    newPlayersInGameMessage players',
    rolesInGameMessage Nothing $ map _role players'
    ] ++ map (newPlayerMessage players') players'
    ++ stageMessages stage' players'
    where
        stage'      = game ^. stage
        players'    = game ^. players

newPlayersInGameMessage :: [Player] -> Message
newPlayersInGameMessage players = publicMessage $ T.concat [
    "A new game of werewolf is starting with ",
    T.intercalate ", " (map _name players), "!"
    ]

newPlayerMessage :: [Player] -> Player -> Message
newPlayerMessage players player
    | isWerewolf player = privateMessage (player ^. name) $ T.intercalate "\n" [T.concat ["You're a Werewolf", packMessage], player ^. role . description]
    | otherwise         = privateMessage (player ^. name) $ T.intercalate "\n" [T.concat ["You're a ", player ^. role . Role.name, "."], player ^. role . description]
    where
        packMessage
            | length (filterWerewolves players) <= 1    = "."
            | otherwise                                 = T.concat [", along with ", T.intercalate ", " (map _name $ filterWerewolves players \\ [player]), "."]

stageMessages :: Stage -> [Player] -> [Message]
stageMessages GameOver _                    = []
stageMessages SeersTurn alivePlayers        = seersTurnMessages . head $ filterSeers alivePlayers
stageMessages Sunrise _                     = [sunriseMessage]
stageMessages Sunset _                      = [nightFallsMessage]
stageMessages VillagesTurn _                = villagesTurnMessages
stageMessages WerewolvesTurn alivePlayers   = werewolvesTurnMessages $ filterWerewolves alivePlayers

seersTurnMessages :: Player -> [Message]
seersTurnMessages seer = [
    publicMessage "The Seer wakes up.",
    privateMessage (seer ^. name) "Whose allegiance would you like to see?"
    ]

sunriseMessage :: Message
sunriseMessage = publicMessage "The sun rises. Everybody wakes up and opens their eyes..."

nightFallsMessage :: Message
nightFallsMessage = publicMessage "Night falls, the village is asleep."

villagesTurnMessages :: [Message]
villagesTurnMessages = [
    publicMessage "As the village gathers in the town square the town clerk calls for a vote.",
    publicMessage "Whom would you like to lynch?"
    ]

werewolvesTurnMessages :: [Player] -> [Message]
werewolvesTurnMessages werewolves = [
    publicMessage "The Werewolves wake up, recognise one another and choose a new victim."
    ] ++ groupMessages (map _name werewolves) "Whom would you like to devour?"

gameOverMessages :: Game -> [Message]
gameOverMessages game = case aliveAllegiances of
    [allegiance]    -> concat [
        [publicMessage $ T.unwords ["The game is over! The", T.pack $ show allegiance, "have won."]],
        map (playerWonMessage . _name) (filter ((allegiance ==) . _allegiance . _role) players'),
        map (playerLostMessage . _name) (filter ((allegiance /=) . _allegiance . _role) players')
        ]
    _               -> publicMessage "The game is over! Everyone died...":map (playerLostMessage . _name) players'
    where
        players'            = game ^. players
        aliveAllegiances    = nub $ map (_allegiance . _role) (filterAlive players')

playerWonMessage :: Text -> Message
playerWonMessage to = privateMessage to "Victory! You won!"

playerLostMessage :: Text -> Message
playerLostMessage to = privateMessage to "Feck, you lost this time round..."

playerQuitMessage :: Player -> Message
playerQuitMessage player = publicMessage $ T.unwords [player ^. name, "the", player ^. role . Role.name, "has quit!"]

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to "Waiting on you..."

pingSeerMessage :: Message
pingSeerMessage = publicMessage "Waiting on the Seer..."

pingWerewolvesMessage :: Message
pingWerewolvesMessage = publicMessage "Waiting on the Werewolves..."

currentStageMessages :: Text -> Stage -> [Message]
currentStageMessages to GameOver    = [gameIsOverMessage to]
currentStageMessages _ Sunrise      = []
currentStageMessages _ Sunset       = []
currentStageMessages to turn        = [privateMessage to $ T.concat [
    "It's currently the ", showTurn turn, " turn."
    ]]
    where
        showTurn :: Stage -> Text
        showTurn GameOver       = undefined
        showTurn SeersTurn      = "Seer's"
        showTurn Sunrise        = undefined
        showTurn Sunset         = undefined
        showTurn VillagesTurn   = "Village's"
        showTurn WerewolvesTurn = "Werewolves'"

rolesInGameMessage :: Maybe Text -> [Role] -> Message
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
playersInGameMessage to players = privateMessage to . T.intercalate "\n" $ [
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

waitingOnMessage :: Maybe Text -> [Player] -> Message
waitingOnMessage mTo players = Message mTo $ T.concat [
    "Waiting on ", T.intercalate ", " playerNames, "..."
    ]
    where
        playerNames = map _name players

playerSeenMessage :: Text -> Player -> Message
playerSeenMessage to target = privateMessage to $ T.concat [
    target ^. name, " is aligned with the ", T.pack . show $ target ^. role . allegiance, "."
    ]

playerMadeLynchVoteMessage :: Text -> Text -> Message
playerMadeLynchVoteMessage voterName targetName = publicMessage $ T.concat [
    voterName, " voted to lynch ", targetName, "."
    ]

playerLynchedMessage :: Player -> Message
playerLynchedMessage player
    | isWerewolf player = publicMessage $ T.unwords [
        player ^. name, "is tied up to a pyre and set alight.",
        "As they scream their body starts to contort and writhe, transforming into a Werewolf.",
        "Thankfully they go limp before breaking free of their restraints."
        ]
    | otherwise         = publicMessage $ T.concat [
        player ^. name, " is tied up to a pyre and set alight.",
        " Eventually the screams start to die and with their last breath,",
        " they reveal themselves as a ", player ^. role . Role.name, "."
        ]

noPlayerLynchedMessage :: Message
noPlayerLynchedMessage = publicMessage $ T.unwords [
    "Daylight is wasted as the townsfolk squabble over whom to tie up.",
    "Looks like no one is being burned this day."
    ]

scapegoatLynchedMessage :: Text -> Message
scapegoatLynchedMessage name = publicMessage $ T.unwords [
    "The townsfolk squabble over whom to tie up. Just as they are about to call it a day",
    "they notice that", name, "has been acting awfully suspicious.",
    "Not wanting to take any chances,", name, "is promptly tied to a pyre and burned alive."
    ]

playerMadeDevourVoteMessage :: Text -> Text -> Text -> Message
playerMadeDevourVoteMessage to voterName targetName = privateMessage to $ T.concat [
    voterName, " voted to devour ", targetName, "."
    ]

playerDevouredMessage :: Player -> Message
playerDevouredMessage player = publicMessage $ T.concat [
    "As you open them you notice a door broken down and ",
    player ^. name, "'s guts half devoured and spilling out over the cobblestones.",
    " From the look of their personal effects, you deduce they were a ",
    player ^. role . Role.name, "."
    ]

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage $ T.unwords [
    "Surprisingly you see everyone present at the town square.",
    "Perhaps the Werewolves have left Miller's Hollow?"
    ]

gameIsOverMessage :: Text -> Message
gameIsOverMessage to = privateMessage to "The game is over!"

playerDoesNotExistMessage :: Text -> Text -> Message
playerDoesNotExistMessage to name = privateMessage to $ T.unwords [
    "Player", name, "does not exist."
    ]

playerCannotDoThatMessage :: Text -> Message
playerCannotDoThatMessage to = privateMessage to "You cannot do that!"

playerCannotDoThatRightNowMessage :: Text -> Message
playerCannotDoThatRightNowMessage to = privateMessage to "You cannot do that right now!"

playerIsDeadMessage :: Text -> Message
playerIsDeadMessage to = privateMessage to "Sshh, you're meant to be dead!"

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to name = privateMessage to $ T.unwords ["Role", name, "does not exist."]

playerHasAlreadyVotedMessage :: Text -> Message
playerHasAlreadyVotedMessage to = privateMessage to "You've already voted!"

targetIsDeadMessage :: Text -> Text -> Message
targetIsDeadMessage to targetName = privateMessage to $ T.unwords [
    targetName, "is already dead!"
    ]

playerCannotDevourAnotherWerewolf :: Text -> Message
playerCannotDevourAnotherWerewolf to = privateMessage to "You cannot devour another Werewolf!"
