{-|
Module      : Game.Werewolf.Game
Description : Simplistic game data structure with lenses.
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A game is not quite as simple as players! Roughly speaking though, this engine is /stateful/. The
game state only changes when a /command/ is issued. Thus, this module defines the 'Game' data
structure and any fields required to keep track of the current state.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Werewolf.Game (
    -- * Game
    Game,
    stage, round, players, boots, allowedVoters, divine, fallenAngelLynched, healUsed,
    hunterRetaliated, jesterRevealed, passed, poison, poisonUsed, priorProtect, protect, roleModel,
    scapegoatBlamed, see, votes,

    Stage(..),
    _DruidsTurn, _GameOver, _HuntersTurn1, _HuntersTurn2, _Lynching, _OraclesTurn, _OrphansTurn,
    _ProtectorsTurn, _ScapegoatsTurn, _SeersTurn, _Sunrise, _Sunset, _VillageDrunksTurn,
    _VillagesTurn, _WerewolvesTurn, _WitchsTurn,

    allStages,
    stageCycle, stageAvailable,

    newGame,

    -- ** Folds
    votee,

    -- ** Prisms
    firstRound, secondRound, thirdRound,

    -- ** Searches
    getAllowedVoters, getPendingVoters,

    -- ** Queries
    hasAnyoneWon, hasFallenAngelWon, hasVillagersWon, hasWerewolvesWon,
) where

import Control.Lens       hiding (isn't)
import Control.Lens.Extra

import           Data.List.Extra
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.String.Humanise
import           Data.Text            (Text)

import Game.Werewolf.Player

import Prelude hiding (round)

-- | There are a few key pieces of information that a game always needs to hold. These are:
--
--   * the 'stage',
--   * the 'round' number and
--   * the 'players'.
--
--   Any further fields on the game are specific to one or more roles (and their respective turns!).
--   Some of the additional fields are reset each round (e.g., the Seer's 'see') while others are
--   kept around for the whole game (e.g., the Orphan's 'roleModel').
data Game = Game
    { _stage              :: Stage
    , _round              :: Int
    , _players            :: [Player]
    , _boots              :: Map Text [Text]
    , _allowedVoters      :: [Text]           -- ^ Jester, Scapegoat
    , _divine             :: Maybe Text       -- ^ Oracle
    , _fallenAngelLynched :: Bool             -- ^ Fallen Angel
    , _healUsed           :: Bool             -- ^ Witch
    , _hunterRetaliated   :: Bool             -- ^ Hunter
    , _jesterRevealed     :: Bool             -- ^ Jester
    , _passed             :: Bool             -- ^ Witch
    , _poison             :: Maybe Text       -- ^ Witch
    , _poisonUsed         :: Bool             -- ^ Witch
    , _priorProtect       :: Maybe Text       -- ^ Protector
    , _protect            :: Maybe Text       -- ^ Protector
    , _roleModel          :: Maybe Text       -- ^ Orphan
    , _scapegoatBlamed    :: Bool             -- ^ Scapegoat
    , _see                :: Maybe Text       -- ^ Seer
    , _votes              :: Map Text Text    -- ^ Villagers and Werewolves
    } deriving (Eq, Read, Show)

-- | Most of these are fairly self-explainable (the turn stages). 'Sunrise' and 'Sunset' are
--   provided as meaningful breaks between the day and night as, for example, a 'VillagesTurn' may
--   not always be available (curse that retched Scapegoat).
--
--   Once the game reaches a turn stage, it requires a /command/ to help push it past. Often only
--   certain roles and commands may be performed at any given stage.
data Stage  = DruidsTurn | GameOver | HuntersTurn1 | HuntersTurn2 | Lynching | OraclesTurn
            | OrphansTurn | ProtectorsTurn | ScapegoatsTurn | SeersTurn | Sunrise | Sunset
            | VillageDrunksTurn | VillagesTurn | WerewolvesTurn | WitchsTurn
    deriving (Eq, Read, Show)

instance Humanise Stage where
    humanise DruidsTurn         = "Druid's turn"
    humanise GameOver           = "Game over"
    humanise HuntersTurn1       = "Hunter's turn"
    humanise HuntersTurn2       = "Hunter's turn"
    humanise Lynching           = "Lynching"
    humanise OraclesTurn        = "Oracle's turn"
    humanise OrphansTurn        = "Orphan's turn"
    humanise ProtectorsTurn     = "Protector's turn"
    humanise ScapegoatsTurn     = "Scapegoat's turn"
    humanise SeersTurn          = "Seer's turn"
    humanise Sunrise            = "Sunrise"
    humanise Sunset             = "Sunset"
    humanise VillageDrunksTurn  = "Village Drunk's turn"
    humanise VillagesTurn       = "village's turn"
    humanise WerewolvesTurn     = "Werewolves' turn"
    humanise WitchsTurn         = "Witch's turn"

makeLenses ''Game

makePrisms ''Stage

-- | All of the 'Stage's in the order that they should occur.
allStages :: [Stage]
allStages =
    [ Sunset
    , OrphansTurn
    , VillageDrunksTurn
    , SeersTurn
    , OraclesTurn
    , ProtectorsTurn
    , WerewolvesTurn
    , WitchsTurn
    , Sunrise
    , HuntersTurn2
    , DruidsTurn
    , VillagesTurn
    , Lynching
    , HuntersTurn1
    , ScapegoatsTurn
    , GameOver
    ]

-- | An infinite cycle of all 'Stage's in the order that they should occur.
stageCycle :: [Stage]
stageCycle = cycle allStages

-- | Checks whether the stage is available for the given 'Game'. Most often this just involves
--   checking if there is an applicable role alive, but sometimes it is more complex.
--
--   One of the more complex checks here is for the 'VillagesTurn'. If the Fallen Angel is in play,
--   then the 'VillagesTurn' is available on the first day rather than only after the first night.
stageAvailable :: Game -> Stage -> Bool
stageAvailable game DruidsTurn          = has (players . druids . alive) game
stageAvailable _ GameOver               = False
stageAvailable game HuntersTurn1        =
    has (players . hunters . dead) game
    && not (game ^. hunterRetaliated)
stageAvailable game HuntersTurn2        =
    has (players . hunters . dead) game
    && not (game ^. hunterRetaliated)
stageAvailable game Lynching            = Map.size (game ^. votes) > 0
stageAvailable game OraclesTurn         = has (players . oracles . alive) game
stageAvailable game OrphansTurn         =
    has (players . orphans . alive) game
    && isNothing (game ^. roleModel)
stageAvailable game ProtectorsTurn      = has (players . protectors . alive) game
stageAvailable game ScapegoatsTurn      = game ^. scapegoatBlamed
stageAvailable game SeersTurn           = has (players . seers . alive) game
stageAvailable _ Sunrise                = True
stageAvailable _ Sunset                 = True
stageAvailable game VillageDrunksTurn   =
    has (players . villageDrunks . alive) game
    && is thirdRound game
stageAvailable game VillagesTurn        = any (is alive) (getAllowedVoters game)
stageAvailable game WerewolvesTurn      = has (players . werewolves . alive) game
stageAvailable game WitchsTurn          =
    has (players . witches . alive) game
    && (not (game ^. healUsed) || not (game ^. poisonUsed))

-- | Creates a new 'Game' with the given players. No validations are performed here, those are left
--   to the binary.
newGame :: [Player] -> Game
newGame players = Game
    { _stage                = head stageCycle
    , _round                = 0
    , _players              = players
    , _boots                = Map.empty
    , _passed               = False
    , _allowedVoters        = players ^.. names
    , _divine               = Nothing
    , _fallenAngelLynched   = False
    , _healUsed             = False
    , _hunterRetaliated     = False
    , _jesterRevealed       = False
    , _poison               = Nothing
    , _poisonUsed           = False
    , _priorProtect         = Nothing
    , _protect              = Nothing
    , _roleModel            = Nothing
    , _scapegoatBlamed      = False
    , _see                  = Nothing
    , _votes                = Map.empty
    }

-- | The traversal of the 'votes' victim's name. This is the player, if they exist, that received
--   the majority of the votes. This could be an empty list depending on whether the votes were in
--   conflict.
votee :: Fold Game Player
votee = folding getVotee

-- | Gets the 'votes' victim's name. This is the player, if they exist, that received the majority
--   of the votes. This could be an empty list depending on whether the votes were in conflict.
getVotee :: Game -> [Player]
getVotee game
    | Map.null (game ^. votes)  = []
    | length result /= 1        = []
    | otherwise                 = game ^.. players . traverse . named (head result)
    where
        votees = Map.elems $ game ^. votes
        result = last $ groupSortOn (length . (`elemIndices` votees)) (nub votees)

-- | The traversal of 'Game's on the first round.
firstRound :: Prism' Game Game
firstRound = prism (set round 0) $ \game -> (if game ^. round == 0 then Right else Left) game

-- | The traversal of 'Game's on the second round.
secondRound :: Prism' Game Game
secondRound = prism (set round 1) $ \game -> (if game ^. round == 1 then Right else Left) game

-- | The traversal of 'Game's on the third round.
thirdRound :: Prism' Game Game
thirdRound = prism (set round 2) $ \game -> (if game ^. round == 2 then Right else Left) game

-- | Gets all the 'allowedVoters' in a game (which is names only) and maps them to their player.
getAllowedVoters :: Game -> [Player]
getAllowedVoters game =
    map (\name -> game ^?! players . traverse . named name) (game ^. allowedVoters)

-- | Gets all 'Alive' players that have yet to vote.
getPendingVoters :: Game -> [Player]
getPendingVoters game =
    game ^.. players . traverse . alive . filtered ((`Map.notMember` votes') . view name)
    where
        votes' = game ^. votes

-- | Queries whether anyone has won.
hasAnyoneWon :: Game -> Bool
hasAnyoneWon game = any ($ game) [hasFallenAngelWon, hasVillagersWon, hasWerewolvesWon]

-- | Queries whether the Fallen Angel has won. The Fallen Angel wins if they manage to get
--   themselves lynched by the Villagers.
hasFallenAngelWon :: Game -> Bool
hasFallenAngelWon game = game ^. fallenAngelLynched

-- | Queries whether the 'Villagers' have won. The 'Villagers' win if they are the only players
--   surviving.
--
--   N.B., the Fallen Angel is not considered when determining whether the 'Villagers' have won.
hasVillagersWon :: Game -> Bool
hasVillagersWon = allOf (players . traverse . alive) (\player -> is villager player || is fallenAngel player)

-- | Queries whether the 'Werewolves' have won. The 'Werewolves' win if they are the only players
--   surviving.
hasWerewolvesWon :: Game -> Bool
hasWerewolvesWon = allOf (players . traverse . alive) (is werewolf)
