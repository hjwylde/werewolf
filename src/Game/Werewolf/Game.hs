{-|
Module      : Game.Werewolf.Game
Description : Game and stage data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Game and stage data structures.
-}

{-# LANGUAGE TemplateHaskell #-}

module Game.Werewolf.Game (
    -- * Game
    Game, stage, round, players, events, passes, heal, healUsed, poison, poisonUsed, priorProtect,
    protect, see, votes,
    newGame,

    -- ** Manipulations
    killPlayer,

    -- ** Queries
    isFirstRound, isGameOver, isDefendersTurn, isSeersTurn, isSunrise, isSunset, isVillagesTurn,
    isWerewolvesTurn, isWitchsTurn, isWolfHoundsTurn, getPassers, getPlayerVote, getPendingVoters,
    getVoteResult,

    -- * Stage
    Stage(..),
    stageCycle, stageAvailable,

    -- * Event
    Event(..),

    -- ** Queries
    getDevourEvent
) where

import Control.Lens

import           Data.List.Extra
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Text       (Text)

import Game.Werewolf.Player

import Prelude hiding (round)

data Game = Game
    { _stage        :: Stage
    , _round        :: Int
    , _players      :: [Player]
    , _events       :: [Event]
    , _passes       :: [Text]
    , _heal         :: Bool
    , _healUsed     :: Bool
    , _poison       :: Maybe Text
    , _poisonUsed   :: Bool
    , _priorProtect :: Maybe Text
    , _protect      :: Maybe Text
    , _see          :: Maybe Text
    , _votes        :: Map Text Text
    } deriving (Eq, Read, Show)

data Stage  = GameOver | DefendersTurn | SeersTurn | Sunrise | Sunset | VillagesTurn
            | WerewolvesTurn | WitchsTurn | WolfHoundsTurn
    deriving (Eq, Read, Show)

data Event = DevourEvent Text | NoDevourEvent | PoisonEvent Text
    deriving (Eq, Read, Show)

makeLenses ''Game

makeLenses ''Stage

newGame :: [Player] -> Game
newGame players = game & stage .~ head (filter (stageAvailable game) stageCycle)
    where
        game = Game
            { _stage        = Sunset
            , _round        = 0
            , _players      = players
            , _events       = []
            , _passes       = []
            , _heal         = False
            , _healUsed     = False
            , _poison       = Nothing
            , _poisonUsed   = False
            , _priorProtect = Nothing
            , _protect      = Nothing
            , _see          = Nothing
            , _votes        = Map.empty
            }

killPlayer :: Game -> Player -> Game
killPlayer game player = game & players %~ map (\player' -> if player' == player then player' & state .~ Dead else player')

isFirstRound :: Game -> Bool
isFirstRound game = game ^. round == 0

isGameOver :: Game -> Bool
isGameOver game = game ^. stage == GameOver

isDefendersTurn :: Game -> Bool
isDefendersTurn game = game ^. stage == DefendersTurn

isSeersTurn :: Game -> Bool
isSeersTurn game = game ^. stage == SeersTurn

isSunrise :: Game -> Bool
isSunrise game = game ^. stage == Sunrise

isSunset :: Game -> Bool
isSunset game = game ^. stage == Sunset

isVillagesTurn :: Game -> Bool
isVillagesTurn game = game ^. stage == VillagesTurn

isWerewolvesTurn :: Game -> Bool
isWerewolvesTurn game = game ^. stage == WerewolvesTurn

isWitchsTurn :: Game -> Bool
isWitchsTurn game = game ^. stage == WitchsTurn

isWolfHoundsTurn :: Game -> Bool
isWolfHoundsTurn game = game ^. stage == WolfHoundsTurn

getPassers :: Game -> [Player]
getPassers game = map (`findByName_` players') passes'
    where
        players'    = game ^. players
        passes'     = game ^. passes

getPlayerVote :: Text -> Game -> Maybe Text
getPlayerVote playerName game = game ^. votes . at playerName

getPendingVoters :: Game -> [Player]
getPendingVoters game = filter (flip Map.notMember votes' . view name) alivePlayers
    where
        votes'          = game ^. votes
        alivePlayers    = filterAlive $ game ^. players

getVoteResult :: Game -> [Player]
getVoteResult game = map (`findByName_` players') result
    where
        players'    = game ^. players
        votees      = Map.elems $ game ^. votes
        result      = last $ groupSortOn (\votee -> length $ elemIndices votee votees) (nub votees)

stageCycle :: [Stage]
stageCycle = cycle
    [ Sunset
    , WolfHoundsTurn
    , SeersTurn
    , DefendersTurn
    , WerewolvesTurn
    , WitchsTurn
    , Sunrise
    , VillagesTurn
    ]

stageAvailable :: Game -> Stage -> Bool
stageAvailable _ GameOver           = False
stageAvailable game DefendersTurn   = any isDefender (filterAlive $ game ^. players)
stageAvailable game SeersTurn       = any isSeer (filterAlive $ game ^. players)
stageAvailable _ Sunrise            = True
stageAvailable _ Sunset             = True
stageAvailable _ VillagesTurn       = True
stageAvailable game WerewolvesTurn  = any isAlignedWithWerewolves (filterAlive $ game ^. players)
stageAvailable game WitchsTurn      =
    any isWitch (filterAlive $ game ^. players)
    && (not (game ^. healUsed) || not (game ^. poisonUsed))
stageAvailable game WolfHoundsTurn  =
    any isWolfHound (filterAlive $ game ^. players)
    && isFirstRound game

getDevourEvent :: Game -> Maybe Event
getDevourEvent game = listToMaybe [event | event@(DevourEvent _) <- game ^. events]
