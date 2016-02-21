{-|
Module      : Game.Werewolf.Internal.Game
Description : Game data structure with functions for manipulating and querying the game state.
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A game is not quite as simple as players! Roughly speaking though, this engine is /stateful/. The
game state only changes when a /command/ is issued (see "Game.Werewolf.Command"). Thus, this module
defines the game data structure and any fields required to keep track of the current state.

It also has a few additional functions for manipulating the game state.
-}

{-# LANGUAGE TemplateHaskell #-}

module Game.Werewolf.Internal.Game (
    -- * Game
    Game,
    stage, round, players, events, passes, allowedVoters, heal, healUsed, poison, poisonUsed,
    priorProtect, protect, roleModel, scapegoatBlamed, see, villageIdiotRevealed, votes,

    Stage(..),
    _DefendersTurn, _GameOver, _ScapegoatsTurn, _SeersTurn, _Sunrise, _Sunset, _UrsussGrunt,
    _VillagesTurn, _WerewolvesTurn, _WildChildsTurn, _WitchsTurn, _WolfHoundsTurn,

    allStages,
    stageCycle, stageAvailable,

    Event(..),
    _DevourEvent, _NoDevourEvent, _PoisonEvent,

    newGame,

    -- ** Manipulations
    killPlayer, setPlayerRole, setPlayerAllegiance,

    -- ** Searches
    getAdjacentAlivePlayers, getPassers, getPlayerVote, getAllowedVoters, getPendingVoters,
    getVoteResult,

    -- ** Queries
    isFirstRound,
    doesPlayerExist,
) where

import Control.Lens

import           Data.List.Extra
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Text       (Text)

import Game.Werewolf.Internal.Player
import Game.Werewolf.Internal.Role   hiding (name)

import Prelude hiding (round)

-- | There are a few key pieces of information that a game always needs to hold. These are:
--
--   * the @stage@,
--   * the @round@ number,
--   * the @players@ and
--   * the @events@.
--
--   Any further fields on the game are specific to one or more roles (and their respective turns!).
--   Some of the additional fields are reset each round (e.g., the Seer's @see@) while others are
--   kept around for the whole game (e.g., the Wild-child's @roleModel@).
--
--   In order to advance a game's state, a 'Game.Werewolf.Command.Command' from a user needs to be received. Afterwards
--   the following steps should be performed:
--
--   1. 'Game.Werewolf.Command.apply' the 'Game.Werewolf.Command.Command'.
--   2. run 'Game.Werewolf.Engine.checkStage'.
--   3. run 'Game.Werewolf.Engine.checkGameOver'.
--
--   'Game.Werewolf.Engine.checkStage' will perform any additional checks and manipulations to the
--   game state before advancing the game's @stage@. It also runs any relevant @events@.
--   'Game.Werewolf.Engine.checkGameOver' will check to see if any of the win conditions are met and
--   if so, advance the game's @stage@ to 'GameOver'.
data Game = Game
    { _stage                :: Stage
    , _round                :: Int
    , _players              :: [Player]
    , _events               :: [Event]
    , _allowedVoters        :: [Text]           -- ^ Scapegoat
    , _heal                 :: Bool             -- ^ Witch
    , _healUsed             :: Bool             -- ^ Witch
    , _passes               :: [Text]           -- ^ Witch
    , _poison               :: Maybe Text       -- ^ Witch
    , _poisonUsed           :: Bool             -- ^ Witch
    , _priorProtect         :: Maybe Text       -- ^ Defender
    , _protect              :: Maybe Text       -- ^ Defender
    , _roleModel            :: Maybe Text       -- ^ Wild-child
    , _scapegoatBlamed      :: Bool             -- ^ Scapegoat
    , _see                  :: Maybe Text       -- ^ Seer
    , _villageIdiotRevealed :: Bool             -- ^ Village Idiot
    , _votes                :: Map Text Text    -- ^ Villagers and Werewolves
    } deriving (Eq, Read, Show)

-- | Most of these are fairly self-sufficient (the turn stages). 'Sunrise' and 'Sunset' are provided
--   as meaningful breaks between the day and night as, for example, a 'VillagesTurn' may not always
--   be available (curse that retched Scapegoat).
--
--   Once the game reaches a turn stage, it requires a 'Game.Werewolf.Command.Command' to help push
--   it past. Often only certain roles and commands may be performed at any given stage.
data Stage  = GameOver | DefendersTurn | ScapegoatsTurn | SeersTurn | Sunrise | Sunset
            | UrsussGrunt | VillagesTurn | WerewolvesTurn | WildChildsTurn | WitchsTurn
            | WolfHoundsTurn
    deriving (Eq, Read, Show)

-- | Events occur /after/ a stage is advanced. This is automatically handled in
--   'Game.Werewolf.Engine.checkStage', while an event's specific behaviour is defined by
--   'Game.Werewolf.Engine.eventAvailable' and 'Game.Werewolf.Engine.applyEvent'.
--
--   For the most part events are used to allow something to happen on a stage different to when it
--   was triggered. E.g., the devour event occurs after the village wakes up rather than when the
--   Werewolves' vote, this gives the Witch a chance to heal the victim.
data Event  = DevourEvent Text  -- ^ Werewolves
            | NoDevourEvent     -- ^ Defender, Werewolves and Witch
            | PoisonEvent Text  -- ^ Witch
    deriving (Eq, Read, Show)

makeLenses ''Game

makePrisms ''Stage

makePrisms ''Event

-- | All of the stages in the order that they should occur.
allStages :: [Stage]
allStages =
    [ VillagesTurn
    , ScapegoatsTurn
    , Sunset
    , SeersTurn
    , WildChildsTurn
    , DefendersTurn
    , WolfHoundsTurn
    , WerewolvesTurn
    , WitchsTurn
    , Sunrise
    , UrsussGrunt
    , GameOver
    ]

-- | An infinite cycle of all stages in the order that they should occur.
stageCycle :: [Stage]
stageCycle = cycle allStages

-- | Checks whether the stage is available for the given game. Most often this just involves
--   checking if there is an applicable role alive, but sometimes it is more complex.
--
--   One of the most complex checks here is for the 'VillagesTurn'. If the Angel is in play, then
--   the 'VillagesTurn' is available on the first day rather than only after the first night.
stageAvailable :: Game -> Stage -> Bool
stageAvailable game DefendersTurn   = has (players . defenders . alive) game
stageAvailable _ GameOver           = False
stageAvailable game ScapegoatsTurn  = game ^. scapegoatBlamed
stageAvailable game SeersTurn       = has (players . seers . alive) game
stageAvailable _ Sunrise            = True
stageAvailable _ Sunset             = True
stageAvailable game UrsussGrunt     = has (players . bearTamers . alive) game
stageAvailable game VillagesTurn    =
    (has (players . angels . alive) game
    || not (isFirstRound game))
    && any (is alive) (getAllowedVoters game)
stageAvailable game WerewolvesTurn  = has (players . werewolves . alive) game
stageAvailable game WildChildsTurn  =
    has (players . wildChildren . alive) game
    && isNothing (game ^. roleModel)
stageAvailable game WitchsTurn      =
    has (players . witches . alive) game
    && (not (game ^. healUsed) || not (game ^. poisonUsed))
stageAvailable game WolfHoundsTurn  = has (players . wolfHounds . alive) game

-- | Creates a new game with the given players. No validations are performed here, those are left to
--   'Game.Werewolf.Engine.startGame'.
newGame :: [Player] -> Game
newGame players = game & stage .~ head (filter (stageAvailable game) stageCycle)
    where
        game = Game
            { _stage                = Sunset
            , _round                = 0
            , _players              = players
            , _events               = []
            , _passes               = []
            , _allowedVoters        = players ^.. names
            , _heal                 = False
            , _healUsed             = False
            , _poison               = Nothing
            , _poisonUsed           = False
            , _priorProtect         = Nothing
            , _protect              = Nothing
            , _roleModel            = Nothing
            , _scapegoatBlamed      = False
            , _see                  = Nothing
            , _villageIdiotRevealed = False
            , _votes                = Map.empty
            }

-- | Kills the given player! This function should be used carefully as it doesn't clear any state
--   that the player's role may use. If you're after just removing a player from a game for a test,
--   try using a 'Game.Werewolf.Command.quitCommand' instead.
killPlayer :: Text -> Game -> Game
killPlayer name' = players . traverse . filteredBy name name' . state .~ Dead

-- | Fudges the player's role by completely setting it to something new. This function is useful for
--   roles such as the Angel where they become something else given some trigger.
setPlayerRole :: Text -> Role -> Game -> Game
setPlayerRole name' role' = players . traverse . filteredBy name name' . role .~ role'

-- | Fudges the player's allegiance. This function is useful for roles such as the Wild-child where
--   they align themselves differently given some trigger.
setPlayerAllegiance :: Text -> Allegiance -> Game -> Game
setPlayerAllegiance name' allegiance' = players . traverse . filteredBy name name' . role . allegiance .~ allegiance'

getAdjacentAlivePlayers :: Text -> Game -> [Player]
getAdjacentAlivePlayers name' game = adjacentAlivePlayers
    where
        players'    = game ^.. players . traverse . alive
        index       = fromJust $ elemIndex name' (players' ^.. names)

        adjacentAlivePlayers
            | index == 0    = last players' : take 2 players'
            | otherwise     = take 3 $ drop (index - 1) (cycle players')

-- | Gets all the @passes@ in a game (which is names only) and maps them to their player.
getPassers :: Game -> [Player]
getPassers game =
    map (\name' -> game ^?! players . traverse . filteredBy name name') (game ^. passes)

-- | Gets a player's vote.
getPlayerVote :: Text -> Game -> Maybe Text
getPlayerVote playerName game = game ^. votes . at playerName

-- | Gets all the @allowedVoters@ in a game (which is names only) and maps them to their player.
getAllowedVoters :: Game -> [Player]
getAllowedVoters game =
    map (\name' -> game ^?! players . traverse . filteredBy name name') (game ^. allowedVoters)

-- | Gets all alive players that have yet to vote.
getPendingVoters :: Game -> [Player]
getPendingVoters game = filter (flip Map.notMember votes' . view name) alivePlayers
    where
        votes'          = game ^. votes
        alivePlayers    = game ^.. players . traverse . alive

-- | Gets all players that had /the/ highest vote count. This could be 1 or more players depending
--   on whether the votes were in conflict.
getVoteResult :: Game -> [Player]
getVoteResult game = map (\name' -> game ^?! players . traverse . filteredBy name name') result
    where
        votees = Map.elems $ game ^. votes
        result = last $ groupSortOn (\votee -> length $ elemIndices votee votees) (nub votees)

-- | @isFirstRound game = game ^. round == 0@
isFirstRound :: Game -> Bool
isFirstRound game = game ^. round == 0

-- | Queries whether the player is in the game.
doesPlayerExist :: Text -> Game -> Bool
doesPlayerExist name = has (players . names . only name)
