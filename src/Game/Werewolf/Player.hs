{-|
Module      : Game.Werewolf.Player
Description : Simplistic player data structure with functions for searching, filtering and querying
              lists of players.
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Players are quite simple in themselves. They have a 'name', 'role' and 'state'. Any complex
behaviour is handled in "Game.Werewolf.Command" and "Game.Werewolf.Engine".
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Werewolf.Player (
    -- * Player
    Player,
    name, role, state,

    State(..),
    _Alive, _Dead,

    newPlayer,

    -- ** Traversals
    angel, devotedServant, druid, jester, protector, scapegoat, seer, simpleVillager,
    simpleWerewolf, villagerVillager, wildChild, witch, wolfHound,
    villager, werewolf,

    -- | These are provided just as a bit of sugar to avoid continually writing @'traverse' .@.
    names, roles, states,

    -- | N.B., these are not legal traversals for the same reason 'filtered' isn't!
    angels, devotedServants, druids, jesters, protectors, scapegoats, seers, simpleVillagers,
    simpleWerewolves, villagerVillagers, wildChildren, witches, wolfHounds,
    villagers, werewolves,
    alive, dead,

    -- * Utility functions
    is, isn't, filteredBy,
) where

import Control.Lens hiding (isn't)

import Data.Function
import Data.Text     (Text)

import Game.Werewolf.Role hiding (name)

-- | A player has a 'name', 'role' and 'state'. Any stateful information needed for a player's
-- @role@ is held on the 'Game' itself.
--
--   N.B., player equality is defined on just the 'name'.
data Player = Player
    { _name  :: Text
    , _role  :: Role
    , _state :: State
    } deriving (Read, Show)

-- | Surprise surprise, players may be 'Dead' or 'Alive'.
data State = Alive | Dead
    deriving (Eq, Read, Show)

makeLenses ''Player

instance Eq Player where
    (==) = (==) `on` view name

makePrisms ''State

-- | Creates a new 'Alive' player.
newPlayer :: Text -> Role -> Player
newPlayer name role = Player name role Alive

-- | The traversal of 'Player's with an 'angelRole'.
--
-- @
-- 'angel' = 'role' . 'only' 'angelRole'
-- @
angel :: Traversal' Player ()
angel = role . only angelRole

-- | The traversal of 'Player's with a 'devotedServantRole'.
--
-- @
-- 'devotedServant' = 'role' . 'only' 'devotedServantRole'
-- @
devotedServant :: Traversal' Player ()
devotedServant = role . only devotedServantRole

-- | The traversal of 'Player's with a 'druidRole'.
--
-- @
-- 'druid' = 'role' . 'only' 'druidRole'
-- @
druid :: Traversal' Player ()
druid = role . only druidRole

-- | The traversal of 'Player's with a 'jesterRole'.
--
-- @
-- 'jester' = 'role' . 'only' 'jesterRole'
-- @
jester :: Traversal' Player ()
jester = role . only jesterRole

-- | The traversal of 'Player's with a 'protectorRole'.
--
-- @
-- 'protector' = 'role' . 'only' 'protectorRole'
-- @
protector :: Traversal' Player ()
protector = role . only protectorRole

-- | The traversal of 'Player's with a 'scapegoatRole'.
--
-- @
-- 'scapegoat' = 'role' . 'only' 'scapegoatRole'
-- @
scapegoat :: Traversal' Player ()
scapegoat = role . only scapegoatRole

-- | The traversal of 'Player's with a 'seerRole'.
--
-- @
-- 'seer' = 'role' . 'only' 'seerRole'
-- @
seer :: Traversal' Player ()
seer = role . only seerRole

-- | The traversal of 'Player's with a 'simpleVillagerRole'.
--
-- @
-- 'simpleVillager' = 'role' . 'only' 'simpleVillagerRole'
-- @
simpleVillager :: Traversal' Player ()
simpleVillager = role . only simpleVillagerRole

-- | The traversal of 'Player's with a 'simpleWerewolfRole'.
--
-- @
-- 'simpleWerewolf' = 'role' . 'only' 'simpleWerewolfRole'
-- @
simpleWerewolf :: Traversal' Player ()
simpleWerewolf = role . only simpleWerewolfRole

-- | The traversal of 'Player's with a 'villagerVillagerRole'.
--
-- @
-- 'villagerVillager' = 'role' . 'only' 'villagerVillagerRole'
-- @
villagerVillager :: Traversal' Player ()
villagerVillager = role . only villagerVillagerRole

-- | The traversal of 'Player's with a 'wildChildRole'.
--
-- @
-- 'wildChild' = 'role' . 'only' 'wildChildRole'
-- @
wildChild :: Traversal' Player ()
wildChild = role . only wildChildRole

-- | The traversal of 'Player's with a 'witchRole'.
--
-- @
-- 'witch' = 'role' . 'only' 'witchRole'
-- @
witch :: Traversal' Player ()
witch = role . only witchRole

-- | The traversal of 'Player's with a 'wolfHoundRole'.
--
-- @
-- 'wolfHound' = 'role' . 'only' 'wolfHoundRole'
-- @
wolfHound :: Traversal' Player ()
wolfHound = role . only wolfHoundRole

-- | The traversal of 'Player's aligned with the 'Villagers'.
--
-- @
-- 'villager' = 'role' . 'allegiance' . '_Villagers'
-- @
villager :: Traversal' Player ()
villager = role . allegiance . _Villagers

-- | The traversal of 'Player's aligned with the 'Werewolves'.
--
-- @
-- 'werewolf' = 'role' . 'allegiance' . '_Werewolves'
-- @
werewolf :: Traversal' Player ()
werewolf = role . allegiance . _Werewolves

-- | This 'Traversal' provides the traversal of 'Player' names.
--
-- @
-- 'names' = 'traverse' . 'name'
-- @
names :: Traversable t => Traversal' (t Player) Text
names = traverse . name

-- | This 'Traversal' provides the traversal of 'Player' roles.
--
-- @
-- 'roles' = 'traverse' . 'role'
-- @
roles :: Traversable t => Traversal' (t Player) Role
roles = traverse . role

-- | This 'Traversal' provides the traversal of 'Player' states.
--
-- @
-- 'states' = 'traverse' . 'state'
-- @
states :: Traversable t => Traversal' (t Player) State
states = traverse . state

-- | This 'Traversal' provides the traversal of 'angel' 'Player's.
--
-- @
-- 'angels' = 'traverse' . 'filtered' ('is' 'angel')
-- @
angels :: Traversable t => Traversal' (t Player) Player
angels = traverse . filtered (is angel)

-- | This 'Traversal' provides the traversal of 'devotedServant' 'Player's.
--
-- @
-- 'devotedServants' = 'traverse' . 'filtered' ('is' 'devotedServant')
-- @
devotedServants :: Traversable t => Traversal' (t Player) Player
devotedServants = traverse . filtered (is devotedServant)

-- | This 'Traversal' provides the traversal of 'druid' 'Player's.
--
-- @
-- 'druids' = 'traverse' . 'filtered' ('is' 'druid')
-- @
druids :: Traversable t => Traversal' (t Player) Player
druids = traverse . filtered (is druid)

-- | This 'Traversal' provides the traversal of 'jester' 'Player's.
--
-- @
-- 'jesters' = 'traverse' . 'filtered' ('is' 'jester')
-- @
jesters :: Traversable t => Traversal' (t Player) Player
jesters = traverse . filtered (is jester)

-- | This 'Traversal' provides the traversal of 'protector' 'Player's.
--
-- @
-- 'protectors' = 'traverse' . 'filtered' ('is' 'protector')
-- @
protectors :: Traversable t => Traversal' (t Player) Player
protectors = traverse . filtered (is protector)

-- | This 'Traversal' provides the traversal of 'scapegoat' 'Player's.
--
-- @
-- 'scapegoats' = 'traverse' . 'filtered' ('is' 'scapegoat')
-- @
scapegoats :: Traversable t => Traversal' (t Player) Player
scapegoats = traverse . filtered (is scapegoat)

-- | This 'Traversal' provides the traversal of 'seer' 'Player's.
--
-- @
-- 'seers' = 'traverse' . 'filtered' ('is' 'seer')
-- @
seers :: Traversable t => Traversal' (t Player) Player
seers = traverse . filtered (is seer)

-- | This 'Traversal' provides the traversal of 'simpleVillager' 'Player's.
--
-- @
-- 'simpleVillagers' = 'traverse' . 'filtered' ('is' 'simpleVillager')
-- @
simpleVillagers :: Traversable t => Traversal' (t Player) Player
simpleVillagers = traverse . filtered (is simpleVillager)

-- | This 'Traversal' provides the traversal of 'simpleWerewolf' 'Player's.
--
-- @
-- 'simpleWerewolves' = 'traverse' . 'filtered' ('is' 'simpleWerewolf')
-- @
simpleWerewolves :: Traversable t => Traversal' (t Player) Player
simpleWerewolves = traverse . filtered (is simpleWerewolf)

-- | This 'Traversal' provides the traversal of 'villagerVillager' 'Player's.
--
-- @
-- 'villagerVillagers' = 'traverse' . 'filtered' ('is' 'villagerVillager')
-- @
villagerVillagers :: Traversable t => Traversal' (t Player) Player
villagerVillagers = traverse . filtered (is villagerVillager)

-- | This 'Traversal' provides the traversal of 'wildChild' 'Player's.
--
-- @
-- 'wildChildren' = 'traverse' . 'filtered' ('is' 'wildChild')
-- @
wildChildren :: Traversable t => Traversal' (t Player) Player
wildChildren = traverse . filtered (is wildChild)

-- | This 'Traversal' provides the traversal of 'witch' 'Player's.
--
-- @
-- 'witches' = 'traverse' . 'filtered' ('is' 'witch')
-- @
witches :: Traversable t => Traversal' (t Player) Player
witches = traverse . filtered (is witch)

-- | This 'Traversal' provides the traversal of 'wolfHound' 'Player's.
--
-- @
-- 'wolfHounds' = 'traverse' . 'filtered' ('is' 'wolfHound')
-- @
wolfHounds :: Traversable t => Traversal' (t Player) Player
wolfHounds = traverse . filtered (is wolfHound)

-- | This 'Traversal' provides the traversal of 'villager' 'Player's.
--
-- @
-- 'villagers' = 'traverse' . 'filtered' ('is' 'villager')
-- @
villagers :: Traversable t => Traversal' (t Player) Player
villagers = traverse . filtered (is villager)

-- | This 'Traversal' provides the traversal of 'werewolf' 'Player's.
--
-- @
-- 'werewolves' = 'traverse' . 'filtered' ('is' 'werewolf')
-- @
werewolves :: Traversable t => Traversal' (t Player) Player
werewolves = traverse . filtered (is werewolf)

-- | This 'Traversal' provides the traversal of 'Alive' 'Player's.
--
-- @
-- 'alive' = 'filtered' ('has' $ 'state' . '_Alive')
-- @
alive :: Traversal' Player Player
alive = filtered (has $ state . _Alive)

-- | This 'Traversal' provides the traversal of 'Dead' 'Player's.
--
-- @
-- 'dead' = 'filtered' ('has' $ 'state' . '_Dead')
-- @
dead :: Traversal' Player Player
dead = filtered (has $ state . _Dead)
