{-|
Module      : Game.Werewolf.Game
Description : Game and turn data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Game and turn data structures.
-}

{-# LANGUAGE TemplateHaskell #-}

module Game.Werewolf.Game (
    -- * Game
    Game(..), turn, players, sees, votes,
    newGame,

    -- ** Manipulations
    killPlayer,

    -- ** Queries
    isSeersTurn, isVillagersTurn, isWerewolvesTurn, isGameOver,

    -- * Turn
    Turn(..),
    turnRotation, turnAvailable,
) where

import Control.Lens

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)

import Game.Werewolf.Player
import Game.Werewolf.Role hiding (Villagers, Werewolves)

data Game = Game
    { _turn    :: Turn
    , _players :: [Player]
    , _sees    :: Map Text Text
    , _votes   :: Map Text Text
    } deriving (Eq, Read, Show)

data Turn = Seers | Villagers | Werewolves | NoOne
    deriving (Eq, Read, Show)

makeLenses ''Game

makeLenses ''Turn

newGame :: [Player] -> Game
newGame players = Game (head $ filter (turnAvailable aliveRoles) turnRotation) players Map.empty Map.empty
    where
        aliveRoles = map _role $ filterAlive players

killPlayer :: Game -> Player -> Game
killPlayer game player = game & players %~ map (\player' -> if player' == player then player' & state .~ Dead else player')

isSeersTurn :: Game -> Bool
isSeersTurn game = game ^. turn == Seers

isVillagersTurn :: Game -> Bool
isVillagersTurn game = game ^. turn == Villagers

isWerewolvesTurn :: Game -> Bool
isWerewolvesTurn game = game ^. turn == Werewolves

isGameOver :: Game -> Bool
isGameOver game = game ^. turn == NoOne

turnRotation :: [Turn]
turnRotation = cycle [Seers, Werewolves, Villagers, NoOne]

turnAvailable :: [Role] -> Turn -> Bool
turnAvailable aliveRoles Seers  = seerRole `elem` aliveRoles
turnAvailable _ Villagers       = True
turnAvailable _ Werewolves      = True
turnAvailable _ NoOne           = False
