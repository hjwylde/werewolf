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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Werewolf.Game (
    -- * Game
    Game,
    variant, stage, round, players, boots, chosenVoters, deadRaised, divine, fallenAngelLynched,
    healUsed, hunterRetaliated, jesterRevealed, marks, passed, poison, poisonUsed, priorProtect,
    protect, roleModel, scapegoatBlamed, see, votes,

    Stage(..),
    _DruidsTurn, _GameOver, _HuntersTurn1, _HuntersTurn2, _Lynching, _NecromancersTurn,
    _OraclesTurn, _OrphansTurn, _ProtectorsTurn, _ScapegoatsTurn, _SeersTurn, _Sunrise, _Sunset,
    _VillageDrunksTurn, _VillagesTurn, _WerewolvesTurn, _WitchsTurn,
    activity,

    allStages,
    stageCycle, stageAvailable,

    newGame,

    -- ** Folds
    votee, allowedVoters, pendingVoters,

    -- ** Prisms
    firstRound, secondRound, thirdRound,

    -- ** Searches
    getMarks,

    -- ** Queries
    hasAnyoneWon, hasDullahanWon, hasFallenAngelWon, hasNecromancerWon, hasVillagersWon,
    hasWerewolvesWon, hasEveryoneLost,
) where

import Control.Lens.Extra

import           Data.List.Extra
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.String.Humanise
import           Data.Text            (Text)

import Game.Werewolf.Player
import Game.Werewolf.Role    hiding (activity, name)
import Game.Werewolf.Variant hiding (name)

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
    { _variant            :: Variant
    , _stage              :: Stage
    , _round              :: Int
    , _players            :: [Player]
    , _boots              :: Map Text [Text]
    , _chosenVoters       :: [Text]           -- ^ Scapegoat
    , _deadRaised         :: Bool             -- ^ Necromancer
    , _divine             :: Maybe Text       -- ^ Oracle
    , _fallenAngelLynched :: Bool             -- ^ Fallen Angel
    , _healUsed           :: Bool             -- ^ Witch
    , _hunterRetaliated   :: Bool             -- ^ Hunter
    , _jesterRevealed     :: Bool             -- ^ Jester
    , _marks              :: [Text]           -- ^ Dullahan
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
data Stage  = DruidsTurn | GameOver | HuntersTurn1 | HuntersTurn2 | Lynching | NecromancersTurn
            | OraclesTurn | OrphansTurn | ProtectorsTurn | ScapegoatsTurn | SeersTurn | Sunrise
            | Sunset | VillageDrunksTurn | VillagesTurn | WerewolvesTurn | WitchsTurn
    deriving (Eq, Read, Show)

instance Humanise Stage where
    humanise DruidsTurn         = "Druid's turn"
    humanise GameOver           = "Game over"
    humanise HuntersTurn1       = "Hunter's turn"
    humanise HuntersTurn2       = "Hunter's turn"
    humanise Lynching           = "Lynching"
    humanise NecromancersTurn   = "Necromancer's turn"
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

activity :: (Functor f, Contravariant f) => (Activity -> f Activity) -> Stage -> f Stage
activity = to getter
    where
        getter DruidsTurn           = Diurnal
        getter GameOver             = Diurnal
        getter HuntersTurn1         = Diurnal
        getter HuntersTurn2         = Diurnal
        getter Lynching             = Diurnal
        getter NecromancersTurn     = Nocturnal
        getter OraclesTurn          = Nocturnal
        getter OrphansTurn          = Nocturnal
        getter ProtectorsTurn       = Nocturnal
        getter ScapegoatsTurn       = Diurnal
        getter SeersTurn            = Nocturnal
        getter Sunrise              = Diurnal
        getter Sunset               = Diurnal
        getter VillageDrunksTurn    = Nocturnal
        getter VillagesTurn         = Diurnal
        getter WerewolvesTurn       = Nocturnal
        getter WitchsTurn           = Nocturnal

-- | All of the 'Stage's in the order that they should occur.
allStages :: [Stage]
allStages =
    [ Sunset
    , OrphansTurn
    , VillageDrunksTurn
    , NecromancersTurn
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
stageAvailable _ Lynching               = True
stageAvailable game NecromancersTurn    =
    has (players . necromancers . alive) game
    && not (game ^. deadRaised)
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
stageAvailable game VillagesTurn        = has allowedVoters game
stageAvailable game WerewolvesTurn      = has (allowedVoters . werewolf) game
stageAvailable game WitchsTurn          =
    has (players . witches . alive) game
    && (not (game ^. healUsed) || not (game ^. poisonUsed))

-- | Creates a new 'Game' with the given players. No validations are performed here, those are left
--   to the binary.
newGame :: Variant -> [Player] -> Game
newGame variant players = Game
    { _variant              = variant
    , _stage                = head stageCycle
    , _round                = 0
    , _players              = players
    , _boots                = Map.empty
    , _passed               = False
    , _chosenVoters         = []
    , _deadRaised           = False
    , _divine               = Nothing
    , _fallenAngelLynched   = False
    , _healUsed             = False
    , _hunterRetaliated     = False
    , _jesterRevealed       = False
    , _marks                = []
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

-- | The traversal of the allowed voters during the 'VillagesTurn' or 'WerewolvesTurn'. In a
--   standard game, this is all 'Alive' players. However there are two scenarios for the
--   'VillagesTurn' that may change this:
--
--   1) if the 'scapegoat' has chosen some 'chosenVoters', it is these players.
--   2) if the 'jester' has been revealed, he may not vote.
allowedVoters :: Fold Game Player
allowedVoters = folding getAllowedVoters

-- | Gets the allowed voters during the 'VillagesTurn' or 'WerewolvesTurn'. In a standard game, this
--   is all 'Alive' players. However there are two scenarios for the 'VillagesTurn' that may change
--   this:
--
--   1) if the 'scapegoat' has chosen some 'chosenVoters', it is these players.
--   2) if the 'jester' has been revealed, he may not vote.
getAllowedVoters :: Game -> [Player]
getAllowedVoters game
    | not . null $ game ^. chosenVoters = filter ((`elem` game ^. chosenVoters) . view name) players'
    | game ^. jesterRevealed            = filter (isn't jester) players'
    | otherwise                         = players'
    where
        players'
            | has (stage . _WerewolvesTurn) game    = game ^.. players . werewolves . alive
            | otherwise                             = game ^.. players . traverse . alive

-- | The traversal of all 'Alive' players that have yet to vote. This is synonymous to @voters -
--   Map.keys votes@
pendingVoters :: Fold Game Player
pendingVoters = folding getPendingVoters

-- | Gets all 'Alive' players that have yet to vote. This is synonymous to @voters - Map.keys
--   votes@
getPendingVoters :: Game -> [Player]
getPendingVoters game = game ^.. allowedVoters . filtered ((`Map.notMember` votes') . view name)
    where
        votes' = game ^. votes

-- | The traversal of 'Game's on the first round.
firstRound :: Prism' Game Game
firstRound = prism (set round 0) $ \game -> (if game ^. round == 0 then Right else Left) game

-- | The traversal of 'Game's on the second round.
secondRound :: Prism' Game Game
secondRound = prism (set round 1) $ \game -> (if game ^. round == 1 then Right else Left) game

-- | The traversal of 'Game's on the third round.
thirdRound :: Prism' Game Game
thirdRound = prism (set round 2) $ \game -> (if game ^. round == 2 then Right else Left) game

-- | Gets all the 'marks' in a game (which is names only) and maps them to their player.
getMarks :: Game -> [Player]
getMarks game = map (\name -> game ^?! players . traverse . named name) (game ^. marks)

-- | Queries whether anyone has won.
hasAnyoneWon :: Game -> Bool
hasAnyoneWon game = any ($ game)
    [ hasDullahanWon
    , hasFallenAngelWon
    , hasNecromancerWon
    , hasVillagersWon
    , hasWerewolvesWon
    ]

-- | Queries whether the Dullahan has won. The Dullahan wins if they manage to eliminate all their
--   marks.
hasDullahanWon :: Game -> Bool
hasDullahanWon game =
    has (players . dullahans . alive) game
    && all (is dead) (getMarks game)

-- | Queries whether the Fallen Angel has won. The Fallen Angel wins if they manage to get
--   themselves lynched by the Villagers.
hasFallenAngelWon :: Game -> Bool
hasFallenAngelWon game = game ^. fallenAngelLynched

-- | Queries whether the Necromancer has won. The 'Necromancer' wins if they and their zombies are
--   the only players surviving.
--
--   N.B., the Jester is not considered when determining whether the 'Necromancer' has won.
hasNecromancerWon :: Game -> Bool
hasNecromancerWon game =
    not (hasEveryoneLost game)
    && allOf (players . traverse . alive)
        (\player -> any ($ player) [is necromancer, is zombie, is jester]) game

-- | Queries whether the 'Villagers' have won. The 'Villagers' win if they are the only players
--   surviving.
--
--   N.B., the Dullahan and Fallen Angel are not considered when determining whether the 'Villagers'
--   have won.
hasVillagersWon :: Game -> Bool
hasVillagersWon game =
    not (hasEveryoneLost game)
    && allOf (players . traverse . alive)
        (\player -> any ($ player) [is villager, is dullahan, is fallenAngel]) game

-- | Queries whether the 'Werewolves' have won. The 'Werewolves' win if they are the only players
--   surviving.
hasWerewolvesWon :: Game -> Bool
hasWerewolvesWon game =
    not (hasEveryoneLost game)
    && allOf (players . traverse . alive) (is werewolf) game

-- | Queries whether everyone has lost.
hasEveryoneLost :: Game -> Bool
hasEveryoneLost = allOf (players . traverse) (is dead)
