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
    findByName, findByName_,

    -- ** Filters
    filterDefenders, filterScapegoats, filterSeers, filterVillagers, filterVillagerVillagers,
    filterWerewolves, filterWitches, filterWolfHounds,
    filterAlignedWithWerewolves,

    -- ** Queries
    doesPlayerExist,
    isDefender, isScapegoat, isSeer, isVillager, isWerewolf, isWitch, isWolfHound,
    isAlignedWithWerewolves,
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

filterDefenders :: [Player] -> [Player]
filterDefenders = filter isDefender

filterScapegoats :: [Player] -> [Player]
filterScapegoats = filter isScapegoat

filterSeers :: [Player] -> [Player]
filterSeers = filter isSeer

filterVillagers :: [Player] -> [Player]
filterVillagers = filter isVillager

filterVillagerVillagers :: [Player] -> [Player]
filterVillagerVillagers = filter isVillagerVillager

filterWerewolves :: [Player] -> [Player]
filterWerewolves = filter isWerewolf

filterWitches :: [Player] -> [Player]
filterWitches = filter isWitch

filterWolfHounds :: [Player] -> [Player]
filterWolfHounds = filter isWolfHound

filterAlignedWithWerewolves :: [Player] -> [Player]
filterAlignedWithWerewolves = filter isAlignedWithWerewolves

doesPlayerExist :: Text -> [Player] -> Bool
doesPlayerExist name = isJust . findByName name

isDefender :: Player -> Bool
isDefender player = player ^. role == defenderRole

isScapegoat :: Player -> Bool
isScapegoat player = player ^. role == scapegoatRole

isSeer :: Player -> Bool
isSeer player = player ^. role == seerRole

isVillager :: Player -> Bool
isVillager player = player ^. role == villagerRole

isVillagerVillager :: Player -> Bool
isVillagerVillager player = player ^. role == villagerVillagerRole

isWerewolf :: Player -> Bool
isWerewolf player = player ^. role == werewolfRole

isWitch :: Player -> Bool
isWitch player = player ^. role == witchRole

isWolfHound :: Player -> Bool
isWolfHound player = player ^. role == wolfHoundRole

isAlignedWithWerewolves :: Player -> Bool
isAlignedWithWerewolves player = player ^. role . allegiance == Werewolves

isAlive :: Player -> Bool
isAlive player = player ^. state == Alive

isDead :: Player -> Bool
isDead player = player ^. state == Dead

filterAlive :: [Player] -> [Player]
filterAlive = filter isAlive

filterDead :: [Player] -> [Player]
filterDead = filter isDead
