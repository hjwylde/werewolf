{-|
Module      : Game.Werewolf.Player
Description : Player data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Player data structures.
-}

{-# LANGUAGE TemplateHaskell #-}

module Game.Werewolf.Player (
    -- * Player
    Player(..), name, role, state,
    newPlayer,

    -- ** Searches
    findByName, findByName_,

    -- ** Filters
    filterSeers, filterVillagers, filterWerewolves, filterScapegoats,

    -- ** Queries
    doesPlayerExist, isSeer, isVillager, isWerewolf, isScapegoat, isAlive, isDead,

    -- * State
    State(..),

    -- ** Filters
    filterAlive, filterDead,
) where

import Control.Lens

import Data.List
import Data.Maybe
import Data.Text  (Text)

import Game.Werewolf.Role hiding (findByName, findByName_, name, _name)

data Player = Player
    { _name  :: Text
    , _role  :: Role
    , _state :: State
    } deriving (Eq, Read, Show)

data State = Alive | Dead
    deriving (Eq, Read, Show)

makeLenses ''Player

newPlayer :: Text -> Role -> Player
newPlayer name role = Player name role Alive

findByName :: Text -> [Player] -> Maybe Player
findByName name = find ((name ==) . _name)

findByName_ :: Text -> [Player] -> Player
findByName_ name = fromJust . findByName name

filterSeers :: [Player] -> [Player]
filterSeers = filter isSeer

filterVillagers :: [Player] -> [Player]
filterVillagers = filter isVillager

filterWerewolves :: [Player] -> [Player]
filterWerewolves = filter isWerewolf

filterScapegoats :: [Player] -> [Player]
filterScapegoats = filter isScapegoat

doesPlayerExist :: Text -> [Player] -> Bool
doesPlayerExist name = isJust . findByName name

isSeer :: Player -> Bool
isSeer player = player ^. role == seerRole

isVillager :: Player -> Bool
isVillager player = player ^. role == villagerRole

isWerewolf :: Player -> Bool
isWerewolf player = player ^. role == werewolfRole

isScapegoat :: Player -> Bool
isScapegoat player = player ^. role == scapegoatRole

isAlive :: Player -> Bool
isAlive player = player ^. state == Alive

isDead :: Player -> Bool
isDead player = player ^. state == Dead

filterAlive :: [Player] -> [Player]
filterAlive = filter isAlive

filterDead :: [Player] -> [Player]
filterDead = filter isDead
