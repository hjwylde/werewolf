{-|
Module      : Game.Werewolf.Response
Description : Response and message data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

A response is used as a return result of calling the @werewolf@ binary. Each response has a list of
associated messages. As such, this module defines these two data structures and a suite of messages
used throughout the werewolf game.

@werewolf@ was designed to be ambivalent to the calling chat client. The response-message structure
reflects this by staying away from anything that could be construed as client-specific. This
includes features such as emoji support.
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
    noGameRunningMessage, gameAlreadyRunningMessage, engineVersionMessage,

    -- ** Generic messages
    newGameMessages, stageMessages, gameOverMessages, playerQuitMessage, gameIsOverMessage,
    playerDoesNotExistMessage, playerCannotDoThatMessage, playerCannotDoThatRightNowMessage,
    playerIsDeadMessage, targetIsDeadMessage, roleDoesNotExistMessage,

    -- ** Ping messages
    pingPlayerMessage, pingRoleMessage,

    -- ** Status messages
    currentStageMessages, playersInGameMessage, waitingOnMessage,

    -- ** Angel's turn messages
    angelJoinedVillagersMessage,

    -- ** Defender's turn messages
    playerCannotProtectSamePlayerTwiceInARowMessage,

    -- ** Seer's turn messages
    playerSeenMessage,

    -- ** Villages' turn messages
    playerMadeLynchVoteMessage, playerLynchedMessage, noPlayerLynchedMessage,
    scapegoatLynchedMessage, villageIdiotLynchedMessage, playerHasAlreadyVotedMessage,

    -- ** Werewolves' turn messages
    playerMadeDevourVoteMessage, playerDevouredMessage, noPlayerDevouredMessage,

    -- ** Wild-child's turn messages
    playerJoinedPackMessage, wildChildJoinedPackMessages, playerCannotChooseSelfMessage,

    -- ** Witch's turn messages
    playerPoisonedMessage, playerHasAlreadyHealedMessage, playerHasAlreadyPoisonedMessage,

    -- ** Werewolves' turn error messages
    playerCannotDevourAnotherWerewolfMessage,

    -- ** Wolf-hound's turn messages
    allegianceDoesNotExistMessage,
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
import           Data.Version

import           Game.Werewolf.Game
import           Game.Werewolf.Player
import           Game.Werewolf.Role   hiding (name)
import qualified Game.Werewolf.Role   as Role
import           GHC.Generics

import qualified System.Exit as Exit

-- | When a user sends a command to the @werewolf@ binary, a response is always returned.
--
--   The chat client interface should then relay any @messages@ from the response. Whether or not
--   the command was valid (indicated by the @ok@ flag) is often irrelevant as the returned
--   @messages@ will include errors to the user.
data Response = Response
    { ok       :: Bool      -- ^ Boolean flag to indicate success.
    , messages :: [Message] -- ^ List of messages.
    } deriving (Eq, Generic, Show)

instance FromJSON Response

instance ToJSON Response where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

-- | A successful, empty response.
success :: Response
success = Response True []

-- | An unsuccessful, empty response.
failure :: Response
failure = Response False []

-- | Exits fast with the given response. The response is encoded as JSON, printed to @stdout@ and
--   then the program is exited with @0@ (success).
--
--   The program always exits with success even if the response was a failure one. This is to
--   distinguish between bad calls to the binary and bad commands to the werewolf engine.
exitWith :: MonadIO m => Response -> m ()
exitWith response = liftIO $ T.putStrLn (T.decodeUtf8 $ encode response) >> Exit.exitSuccess

data Message = Message
    { to      :: Maybe Text -- ^ The message recipient: 'Nothing' for a public message,
                            --   'Just' for a private message.
    , message :: Text       -- ^ The message text.
    } deriving (Eq, Generic, Show)

instance FromJSON Message

instance ToJSON Message where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

-- | Creates a public message with the given text.
publicMessage :: Text -> Message
publicMessage = Message Nothing

-- | @privateMessage to message@
--
--   Creates a private message to @to@ with the given text.
privateMessage :: Text -> Text -> Message
privateMessage to = Message (Just to)

-- | @groupmessages tos message@
--
--   Creates multiple private messages (1 to each recipient) with the given text.
groupMessages :: [Text] -> Text -> [Message]
groupMessages tos message = map (`privateMessage` message) tos

noGameRunningMessage :: Text -> Message
noGameRunningMessage to = privateMessage to "No game is running."

gameAlreadyRunningMessage :: Text -> Message
gameAlreadyRunningMessage to = privateMessage to "A game is already running."

engineVersionMessage :: Text -> Version -> Message
engineVersionMessage to version = privateMessage to $ T.unwords ["Version", T.pack $ showVersion version]

newGameMessages :: Game -> [Message]
newGameMessages game = concat
    [ [newPlayersInGameMessage players', rolesInGameMessage $ map (view role) players']
    , map newPlayerMessage players'
    , villagerVillagerMessages
    , stageMessages game
    ]
    where
        players'                    = game ^. players
        villagerVillagerMessages    =
            case findByRole villagerVillagerRole players' of
                Just villagerVillager   -> [villagerVillagerMessage $ villagerVillager ^. name]
                _                       -> []

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
    VillagesTurn    -> if isFirstRound game
        then firstVillagesTurnMessages
        else villagesTurnMessages
    WerewolvesTurn  -> if isFirstRound game
        then firstWerewolvesTurnMessages aliveWerewolfNames
        else werewolvesTurnMessages aliveWerewolfNames
    WildChildsTurn  -> wildChildsTurnMessages wildChildsName
    WitchsTurn      -> witchsTurnMessages game
    WolfHoundsTurn  -> wolfHoundsTurnMessages wolfHoundsName
    where
        players'            = game ^. players
        defendersName       = findByRole_ defenderRole players' ^. name
        seersName           = findByRole_ seerRole players' ^. name
        aliveWerewolfNames  = map (view name) . filterAlive $ filterWerewolves players'
        wildChildsName      = findByRole_ wildChildRole players' ^. name
        wolfHoundsName      = findByRole_ wolfHoundRole players' ^. name

defendersTurnMessages :: Text -> [Message]
defendersTurnMessages to =
    [ publicMessage "The Defender wakes up."
    , privateMessage to "Whom would you like to `protect`?"
    ]

seersTurnMessages :: Text -> [Message]
seersTurnMessages to =
    [ publicMessage "The Seer wakes up."
    , privateMessage to "Whose allegiance would you like to `see`?"
    ]

sunriseMessage :: Message
sunriseMessage = publicMessage "The sun rises. Everybody wakes up and opens their eyes..."

nightFallsMessage :: Message
nightFallsMessage = publicMessage "Night falls, the village is asleep."

firstVillagesTurnMessages :: [Message]
firstVillagesTurnMessages =
    ( publicMessage $ T.unwords
        [ "Alas, again I regrettably yield advice: an angelic menace walks among you."
        , "Do not cast your votes lightly,"
        , "for he will relish in this opportunity to be free from his terrible nightmare."
        ]
    ) : villagesTurnMessages

villagesTurnMessages :: [Message]
villagesTurnMessages =
    [ publicMessage "As the village gathers in the square the town clerk calls for a vote."
    , publicMessage "Whom would you like to `vote` to lynch?"
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
    ] ++ groupMessages tos "Whom would you like to `vote` to devour?"

wildChildsTurnMessages :: Text -> [Message]
wildChildsTurnMessages to =
    [ publicMessage "The Wild-child wakes up."
    , privateMessage to "Whom do you `choose` to be your role model?"
    ]

witchsTurnMessages :: Game -> [Message]
witchsTurnMessages game = concat
    [ [wakeUpMessage]
    , devourMessages
    , healMessages
    , poisonMessages
    , [passMessage]
    ]
    where
        witchsName      = findByRole_ witchRole (game ^. players) ^. name
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
                && isJust (getDevourEvent game) = [privateMessage witchsName "Would you like to `heal` them?"]
            | otherwise                         = []
        poisonMessages
            | not (game ^. poisonUsed)          = [privateMessage witchsName "Would you like to `poison` anyone?"]
            | otherwise                         = []

wolfHoundsTurnMessages :: Text -> [Message]
wolfHoundsTurnMessages to =
    [ publicMessage "The Wolf-hound wakes up."
    , privateMessage to "Which allegiance do you `choose` to be aligned with?"
    ]

gameOverMessages :: Game -> [Message]
gameOverMessages game
    | any isAngel (filterDead $ game ^. players)    =
        concat
            [ [publicMessage "You should have heeded my warning, for now the Angel has been set free!"]
            , [publicMessage "The game is over! The Angel has won."]
            , [playerWonMessage $ angel ^. name]
            , map (playerLostMessage . view name) (players' \\ [angel])
            ]
    | length aliveAllegiances == 1                  = do
        let allegiance' = head aliveAllegiances

        concat
            [ [publicMessage $ T.unwords ["The game is over! The", T.pack $ show allegiance', "have won."]]
            , map (playerWonMessage . view name) (filter ((allegiance' ==) . view (role . allegiance)) players')
            , map (playerLostMessage . view name) (filter ((allegiance' /=) . view (role . allegiance)) players')
            ]
    | otherwise                                     = publicMessage "The game is over! Everyone died...":map (playerLostMessage . view name) players'
    where
        players'            = game ^. players
        angel               = findByRole_ angelRole players'
        aliveAllegiances    = nub $ map (view $ role . allegiance) (filterAlive players')

playerWonMessage :: Text -> Message
playerWonMessage to = privateMessage to "Victory! You won!"

playerLostMessage :: Text -> Message
playerLostMessage to = privateMessage to "Feck, you lost this time round..."

playerQuitMessage :: Player -> Message
playerQuitMessage player = publicMessage $ T.unwords [player ^. name, "the", player ^. role . Role.name, "has quit!"]

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

targetIsDeadMessage :: Text -> Text -> Message
targetIsDeadMessage to targetName = privateMessage to $ T.unwords [targetName, "is already dead!"]

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to name = privateMessage to $ T.unwords ["Role", name, "does not exist."]

pingPlayerMessage :: Text -> Message
pingPlayerMessage to = privateMessage to "Waiting on you..."

pingRoleMessage :: Text -> Message
pingRoleMessage roleName = publicMessage $ T.concat ["Waiting on the ", roleName, "..."]

currentStageMessages :: Text -> Stage -> [Message]
currentStageMessages to GameOver    = [gameIsOverMessage to]
currentStageMessages _ Sunrise      = []
currentStageMessages _ Sunset       = []
currentStageMessages to turn        = [privateMessage to $ T.concat
    [ "It's currently the ", showTurn turn, " turn."
    ]]
    where
        showTurn :: Stage -> Text
        showTurn DefendersTurn  = "Defender's"
        showTurn GameOver       = undefined
        showTurn SeersTurn      = "Seer's"
        showTurn Sunrise        = undefined
        showTurn Sunset         = undefined
        showTurn VillagesTurn   = "Village's"
        showTurn WerewolvesTurn = "Werewolves'"
        showTurn WildChildsTurn = "Wild-child's"
        showTurn WitchsTurn     = "Witch's"
        showTurn WolfHoundsTurn = "Wolf-hound's"

rolesInGameMessage :: [Role] -> Message
rolesInGameMessage roles = publicMessage $ T.concat
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

angelJoinedVillagersMessage :: Message
angelJoinedVillagersMessage = publicMessage $ T.unwords
    [ "You hear the Angel wrought with anger off in the distance."
    , "He failed to attract the discriminatory vote of the village"
    , "or the devouring vindictiveness of the lycanthropes."
    , "Now he is stuck here, doomed forever to live out a mortal life as a Simple Villager."
    ]

playerCannotProtectSamePlayerTwiceInARowMessage :: Text -> Message
playerCannotProtectSamePlayerTwiceInARowMessage to =
    privateMessage to "You cannot protect the same player twice in a row!"

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
    | isWerewolf player
        && not (isWildChild player) = publicMessage $ T.concat
        [ playerName, " is tied up to a pyre and set alight."
        , " As they scream their body starts to contort and writhe, transforming into "
        , article playerRole, " ", playerRole ^. Role.name, "."
        , " Thankfully they go limp before breaking free of their restraints."
        ]
    | otherwise                     = publicMessage $ T.concat
        [ playerName, " is tied up to a pyre and set alight."
        , " Eventually the screams start to die and with their last breath,"
        , " they reveal themselves as "
        , article playerRole, " ", playerRole ^. Role.name, "."
        ]
    where
        playerName = player ^. name
        playerRole = player ^. role

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

villageIdiotLynchedMessage :: Text -> Message
villageIdiotLynchedMessage name = publicMessage $ T.unwords
    [ "Just as the townsfolk tie", name, "up to the pyre, a voice in the crowd yells out."
    , "\"We can't burn", name, "! He's that oaf, you know, John's boy!\""
    , "The Village Idiot is quickly untied and apologised to."
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

noPlayerDevouredMessage :: Message
noPlayerDevouredMessage = publicMessage $ T.unwords
    [ "Surprisingly you see everyone present at the town square."
    , "Perhaps the Werewolves have left Miller's Hollow?"
    ]

playerJoinedPackMessage :: Text -> [Text] -> Message
playerJoinedPackMessage to werewolfNames = privateMessage to $ T.unwords
    [ "The death of your role model is distressing."
    , "Without second thought you abandon the Villagers and run off into the woods,"
    , "towards your old home."
    , "As you arrive you see the familiar faces of", T.intercalate ", " werewolfNames
    , "waiting and happy to have you back."
    ]

wildChildJoinedPackMessages :: [Text] -> Text -> [Message]
wildChildJoinedPackMessages tos wildChildsName = groupMessages tos $ T.unwords
    [ wildChildsName, "the Wild-child scampers off into the woods."
    , "Without his role model nothing is holding back his true, wolfish, nature."
    ]

playerCannotChooseSelfMessage :: Text -> Message
playerCannotChooseSelfMessage to = privateMessage to "You cannot choose yourself!"

playerPoisonedMessage :: Player -> Message
playerPoisonedMessage player = publicMessage $ T.unwords
    [ "Upon further discovery, it looks like the Witch struck in the night."
    , player ^. name, "the", player ^. role . Role.name
    , "is lying in their bed, poisoned, drooling over the side."
    ]

playerHasAlreadyHealedMessage :: Text -> Message
playerHasAlreadyHealedMessage to = privateMessage to "You've already healed someone!"

playerHasAlreadyPoisonedMessage :: Text -> Message
playerHasAlreadyPoisonedMessage to = privateMessage to "You've already poisoned someone!"

playerCannotDevourAnotherWerewolfMessage :: Text -> Message
playerCannotDevourAnotherWerewolfMessage to =
    privateMessage to "You cannot devour another Werewolf!"

allegianceDoesNotExistMessage :: Text -> Text -> Message
allegianceDoesNotExistMessage to name = privateMessage to $ T.unwords
    [ "Allegiance", name, "does not exist."
    ]

article :: Role -> Text
article role
    | role `elem` restrictedRoles   = "the"
    | otherwise                     = "a"
