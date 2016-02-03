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
    Game(..), stage, players, events, passes, poison, see, votes,
    newGame,

    -- ** Manipulations
    killPlayer,

    -- ** Queries
    isGameOver, isSeersTurn, isSunrise, isSunset, isVillagesTurn, isWerewolvesTurn, isWitchsTurn,
    getPassers, getPlayerVote, getPendingVoters, getVoteResult,

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

data Game = Game
    { _stage   :: Stage
    , _players :: [Player]
    , _events  :: [Event]
    , _passes  :: [Text]
    , _poison  :: Maybe Text
    , _see     :: Maybe Text
    , _votes   :: Map Text Text
    } deriving (Eq, Read, Show)

data Stage = GameOver | SeersTurn | Sunrise | Sunset | VillagesTurn | WerewolvesTurn | WitchsTurn
    deriving (Eq, Read, Show)

data Event = DevourEvent Text | PoisonEvent Text
    deriving (Eq, Read, Show)

makeLenses ''Game

makeLenses ''Stage

newGame :: [Player] -> Game
newGame players = Game stage players [] [] Nothing Nothing Map.empty
    where
        stage = head $ filter (stageAvailable players []) stageCycle

killPlayer :: Game -> Player -> Game
killPlayer game player = game & players %~ map (\player' -> if player' == player then player' & state .~ Dead else player')

isGameOver :: Game -> Bool
isGameOver game = game ^. stage == GameOver

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

getPassers :: Game -> [Player]
getPassers game = map (`findByName_` players') passes'
    where
        players'    = game ^. players
        passes'     = game ^. passes

getPlayerVote :: Text -> Game -> Maybe Text
getPlayerVote playerName game = game ^. votes . at playerName

getPendingVoters :: Game -> [Player]
getPendingVoters game = filter (flip Map.notMember votes' . _name) alivePlayers
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
stageCycle = cycle [Sunset, SeersTurn, WerewolvesTurn, WitchsTurn, Sunrise, VillagesTurn]

stageAvailable :: [Player] -> [Event] -> Stage -> Bool
stageAvailable _ _ GameOver                 = False
stageAvailable players _ SeersTurn          = any isSeer (filterAlive players)
stageAvailable _ _ Sunrise                  = True
stageAvailable _ _ Sunset                   = True
stageAvailable _ _ VillagesTurn             = True
stageAvailable players _ WerewolvesTurn     = any isWerewolf (filterAlive players)
stageAvailable players events WitchsTurn    =
    any isWitch (filterAlive players) &&
    (witch ^. name `notElem` [name | (DevourEvent name) <- events])
    where
        witch = head $ filterWitches players

getDevourEvent :: Game -> Maybe Event
getDevourEvent game = listToMaybe [event | event@(DevourEvent _) <- game ^. events]
