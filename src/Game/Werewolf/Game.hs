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
    Game(..), stage, players, see, votes,
    newGame,

    -- ** Manipulations
    killPlayer,

    -- ** Queries
    isGameOver, isSeersTurn, isSunset, isVillagesTurn, isWerewolvesTurn,

    -- * Stage
    Stage(..),
    stageCycle, stageAvailable,
) where

import Control.Lens

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)

import Game.Werewolf.Player
import Game.Werewolf.Role   hiding (Villagers, Werewolves)

data Game = Game
    { _stage   :: Stage
    , _players :: [Player]
    , _see     :: Maybe Text
    , _votes   :: Map Text Text
    } deriving (Eq, Read, Show)

data Stage = GameOver | SeersTurn | Sunrise | Sunset | VillagesTurn | WerewolvesTurn
    deriving (Eq, Read, Show)

makeLenses ''Game

makeLenses ''Stage

newGame :: [Player] -> Game
newGame players = Game stage players Nothing Map.empty
    where
        stage       = head $ filter (stageAvailable aliveRoles) stageCycle
        aliveRoles  = map _role $ filterAlive players

killPlayer :: Game -> Player -> Game
killPlayer game player = game & players %~ map (\player' -> if player' == player then player' & state .~ Dead else player')

isGameOver :: Game -> Bool
isGameOver game = game ^. stage == GameOver

isSeersTurn :: Game -> Bool
isSeersTurn game = game ^. stage == SeersTurn

isSunset :: Game -> Bool
isSunset game = game ^. stage == Sunset

isVillagesTurn :: Game -> Bool
isVillagesTurn game = game ^. stage == VillagesTurn

isWerewolvesTurn :: Game -> Bool
isWerewolvesTurn game = game ^. stage == WerewolvesTurn

stageCycle :: [Stage]
stageCycle = cycle [Sunset, SeersTurn, WerewolvesTurn, Sunrise, VillagesTurn]

stageAvailable :: [Role] -> Stage -> Bool
stageAvailable _ GameOver                   = False
stageAvailable aliveRoles SeersTurn         = seerRole `elem` aliveRoles
stageAvailable _ Sunrise                    = True
stageAvailable _ Sunset                     = True
stageAvailable _ VillagesTurn               = True
stageAvailable aliveRoles WerewolvesTurn    = werewolfRole `elem` aliveRoles
