{-|
Module      : Game.Werewolf.Player
Description : Simplistic player data structure with functions for searching, filtering and querying
              lists of players.
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Players are quite simple in themselves. They have a @name@, @role@ and @state@. Any complex
behaviour is handled in "Game.Werewolf.Command" and "Game.Werewolf.Engine". This module provides
utility functions for searching, filtering and querying lists of players based on these 3
attributes.
-}

{-# LANGUAGE TemplateHaskell #-}

module Game.Werewolf.Player (
    -- * Player
    Player, name, role, state,

    State(..),

    newPlayer,

    -- ** Searches
    findByName, findByName_, findByRole, findByRole_,

    -- ** Filters
    filterByRole,
    filterWerewolves,
    filterAlive, filterDead,

    -- ** Queries
    doesPlayerExist,
    isAngel, isDefender, isScapegoat, isSeer, isSimpleVillager, isSimpleWerewolf, isVillageIdiot,
    isVillagerVillager, isWildChild, isWitch, isWolfHound,
    isVillager, isWerewolf,
    isAlive, isDead,
) where

import Control.Lens

import Data.Function
import Data.List
import Data.Maybe
import Data.Text     (Text)

import Game.Werewolf.Role hiding (name)

-- | A player has a @name@, @role@ and @state@. Any stateful information needed for a player's role
--   is held on the 'Game' itself.
--
--   N.B., player equality is defined on just the @name@ as a player's @role@ may change throughout
--   the game.
data Player = Player
    { _name  :: Text
    , _role  :: Role
    , _state :: State
    } deriving (Read, Show)

-- | Surprise surprise, players may be dead or alive.
data State = Alive | Dead
    deriving (Eq, Read, Show)

makeLenses ''Player

instance Eq Player where
    (==) = (==) `on` view name

-- | Creates a new 'Alive' player.
newPlayer :: Text -> Role -> Player
newPlayer name role = Player name role Alive

-- | Attempts to find the first player in the list with the given name.
findByName :: Text -> [Player] -> Maybe Player
findByName name' = find ((name' ==) . view name)

-- | Finds the first player in the list with the given name.
--
--   @findByName_ name = fromJust . findByName name@
findByName_ :: Text -> [Player] -> Player
findByName_ name = fromJust . findByName name

-- | Attempts to find the first player in the list with the given role.
findByRole :: Role -> [Player] -> Maybe Player
findByRole role' = find ((role' ==) . view role)

-- | Finds the first player in the list with the given role.
--
--   @findByRole_ role = fromJust . findByRole role@
findByRole_ :: Role -> [Player] -> Player
findByRole_ role = fromJust . findByRole role

-- | Filters players by role.
filterByRole :: Role -> [Player] -> [Player]
filterByRole role' = filter ((role' ==) . view role)

-- | Filters players by allegiance, not role.
--   If you're after filtering by role, try @filterByRole simpleWerewolfRole@.
--
--   @filterWerewolves = filter isWerewolf@
filterWerewolves :: [Player] -> [Player]
filterWerewolves = filter isWerewolf

-- | @filterAlive = filter isAlive@
filterAlive :: [Player] -> [Player]
filterAlive = filter isAlive

-- | @filterDead = filter isDead@
filterDead :: [Player] -> [Player]
filterDead = filter isDead

-- | @doesPlayerExist name = isJust . findByName name@
doesPlayerExist :: Text -> [Player] -> Bool
doesPlayerExist name = isJust . findByName name

-- | @isAngel player = player ^. role == 'angelRole'@
isAngel :: Player -> Bool
isAngel player = player ^. role == angelRole

-- | @isDefender player = player ^. role == 'defenderRole'@
isDefender :: Player -> Bool
isDefender player = player ^. role == defenderRole

-- | @isScapegoat player = player ^. role == 'scapegoatRole'@
isScapegoat :: Player -> Bool
isScapegoat player = player ^. role == scapegoatRole

-- | @isSeer player = player ^. role == 'seerRole'@
isSeer :: Player -> Bool
isSeer player = player ^. role == seerRole

-- | @isSimpleVillager player = player ^. role == 'simpleVillagerRole'@
isSimpleVillager :: Player -> Bool
isSimpleVillager player = player ^. role == simpleVillagerRole

-- | @isSimpleWerewolf player = player ^. role == 'simpleWerewolfRole'@
isSimpleWerewolf :: Player -> Bool
isSimpleWerewolf player = player ^. role == simpleWerewolfRole

-- | @isVillageIdiot player = player ^. role == 'villageIdiotRole'@
isVillageIdiot :: Player -> Bool
isVillageIdiot player = player ^. role == villageIdiotRole

-- | @isVillagerVillager player = player ^. role == 'villagerVillagerRole'@
isVillagerVillager :: Player -> Bool
isVillagerVillager player = player ^. role == villagerVillagerRole

-- | @isWildChild player = player ^. role == 'wildChildRole'@
isWildChild :: Player -> Bool
isWildChild player = player ^. role == wildChildRole

-- | @isWitch player = player ^. role == 'witchRole'@
isWitch :: Player -> Bool
isWitch player = player ^. role == witchRole

-- | @isWolfHound player = player ^. role == 'wolfHoundRole'@
isWolfHound :: Player -> Bool
isWolfHound player = player ^. role == wolfHoundRole

-- | Queries a player's allegiance, not role.
--   If you're after querying their role, try 'isSimpleVillager'.
--
--   @isVillager player = player ^. role . allegiance == Villagers@
isVillager :: Player -> Bool
isVillager player = player ^. role . allegiance == Villagers

-- | Queries a player's allegiance, not role.
--   If you're after querying their role, try 'isSimpleWerewolf'.
--
--   @isWerewolf player = player ^. role . allegiance == Werewolves@
isWerewolf :: Player -> Bool
isWerewolf player = player ^. role . allegiance == Werewolves

-- | @isAlive player = player ^. state == Alive@
isAlive :: Player -> Bool
isAlive player = player ^. state == Alive

-- | @isDead player = player ^. state == Dead@
isDead :: Player -> Bool
isDead player = player ^. state == Dead
