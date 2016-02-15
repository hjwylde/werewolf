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
    Player, name, role, state,
    newPlayer,

    -- ** Searches
    findByName, findByName_, findByRole, findByRole_,

    -- ** Filters
    filterByRole,
    filterWerewolves,

    -- ** Queries
    doesPlayerExist,
    isDefender, isScapegoat, isSeer, isSimpleVillager, isSimpleWerewolf, isVillagerVillager,
    isWildChild, isWitch, isWolfHound,
    isWerewolf,
    isAlive, isDead,

    -- * State
    State(..),

    -- ** Filters
    filterAlive, filterDead,
) where

import Control.Lens

import Data.List
import Data.Maybe
import Data.Text  (Text)

import Game.Werewolf.Role hiding (name)

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
findByName name' = find ((name' ==) . view name)

findByName_ :: Text -> [Player] -> Player
findByName_ name = fromJust . findByName name

findByRole :: Role -> [Player] -> Maybe Player
findByRole role' = find ((role' ==) . view role)

findByRole_ :: Role -> [Player] -> Player
findByRole_ role = fromJust . findByRole role

filterByRole :: Role -> [Player] -> [Player]
filterByRole role' = filter ((role' ==) . view role)

filterWerewolves :: [Player] -> [Player]
filterWerewolves = filter isWerewolf

doesPlayerExist :: Text -> [Player] -> Bool
doesPlayerExist name = isJust . findByName name

isDefender :: Player -> Bool
isDefender player = player ^. role == defenderRole

isScapegoat :: Player -> Bool
isScapegoat player = player ^. role == scapegoatRole

isSeer :: Player -> Bool
isSeer player = player ^. role == seerRole

isSimpleVillager :: Player -> Bool
isSimpleVillager player = player ^. role == simpleVillagerRole

isSimpleWerewolf :: Player -> Bool
isSimpleWerewolf player = player ^. role == simpleWerewolfRole

isVillagerVillager :: Player -> Bool
isVillagerVillager player = player ^. role == villagerVillagerRole

isWildChild :: Player -> Bool
isWildChild player = player ^. role == wildChildRole

isWitch :: Player -> Bool
isWitch player = player ^. role == witchRole

isWolfHound :: Player -> Bool
isWolfHound player = player ^. role == wolfHoundRole

isWerewolf :: Player -> Bool
isWerewolf player = player ^. role . allegiance == Werewolves

isAlive :: Player -> Bool
isAlive player = player ^. state == Alive

isDead :: Player -> Bool
isDead player = player ^. state == Dead

filterAlive :: [Player] -> [Player]
filterAlive = filter isAlive

filterDead :: [Player] -> [Player]
filterDead = filter isDead
