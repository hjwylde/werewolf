{-|
Module      : Game.Werewolf.Internal.Player
Description : Simplistic player data structure with functions for searching, filtering and querying
              lists of players.
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Players are quite simple in themselves. They have a 'name', 'role' and 'state'. Any complex
behaviour is handled in "Game.Werewolf.Command" and "Game.Werewolf.Engine". This module provides
utility functions for searching, filtering and querying lists of players based on these 3
attributes.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Werewolf.Internal.Player (
    -- * Player
    Player,
    name, role, state,

    State(..),
    _Alive, _Dead,

    newPlayer,

    -- ** Prisms
    -- | N.B., these are not legal traversals for the same reason 'filtered' isn't!
    angel, bearTamer, defender, scapegoat, seer, simpleVillager, simpleWerewolf, villageIdiot,
    villagerVillager, wildChild, witch, wolfHound,
    villager, werewolf,
    alive, dead,

    -- ** Traversals
    -- | These are provided just as a bit of sugar to avoid continually writing @'traverse' .@.
    names, roles, states,

    -- | N.B., these are not legal traversals for the same reason 'filtered' isn't!
    angels, bearTamers, defenders, scapegoats, seers, simpleVillagers, simpleWerewolves,
    villageIdiots, villagerVillagers, wildChildren, witches, wolfHounds,
    villagers, werewolves,

    -- * Utility functions
    is, filteredBy,
) where

import Control.Lens

import Data.Function
import Data.Text     (Text)

import Game.Werewolf.Internal.Role hiding (name)

-- | A player has a 'name', 'role' and 'state'. Any stateful information needed for a player's role
--   is held on the 'Game' itself.
--
--   N.B., player equality is defined on just the 'name'.
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

makePrisms ''State

-- | Creates a new 'Alive' player.
newPlayer :: Text -> Role -> Player
newPlayer name role = Player name role Alive

-- | This 'Prism' provides the traversal of a 'Player' with an 'angelRole'.
angel :: Prism' Player Player
angel = genericRolePrism angelRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'bearTamerRole'.
bearTamer :: Prism' Player Player
bearTamer = genericRolePrism bearTamerRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'defenderRole'.
defender :: Prism' Player Player
defender = genericRolePrism defenderRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'scapegoatRole'.
scapegoat :: Prism' Player Player
scapegoat = genericRolePrism scapegoatRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'seerRole'.
seer :: Prism' Player Player
seer = genericRolePrism seerRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'simpleVillagerRole'.
simpleVillager :: Prism' Player Player
simpleVillager = genericRolePrism simpleVillagerRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'simpleWerewolfRole'.
simpleWerewolf :: Prism' Player Player
simpleWerewolf = genericRolePrism simpleWerewolfRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'villageIdiotRole'.
villageIdiot :: Prism' Player Player
villageIdiot = genericRolePrism villageIdiotRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'villagerVillagerRole'.
villagerVillager :: Prism' Player Player
villagerVillager = genericRolePrism villagerVillagerRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'wildChildRole'.
wildChild :: Prism' Player Player
wildChild = genericRolePrism wildChildRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'witchRole'.
witch :: Prism' Player Player
witch = genericRolePrism witchRole

-- | This 'Prism' provides the traversal of a 'Player' with a 'wolfHoundRole'.
wolfHound :: Prism' Player Player
wolfHound = genericRolePrism wolfHoundRole

genericRolePrism :: Role -> Prism' Player Player
genericRolePrism role' = prism (set role role') $ \player ->
    if player ^. role == role' then Right player else Left player

-- | This 'Prism' provides the traversal of a 'Player' aligned with the 'Villagers'.
villager :: Prism' Player Player
villager = genericRoleAllegiancePrism Villagers

-- | This 'Prism' provides the traversal of a 'Player' aligned with the 'Werewolves'.
werewolf :: Prism' Player Player
werewolf = genericRoleAllegiancePrism Werewolves

genericRoleAllegiancePrism :: Allegiance -> Prism' Player Player
genericRoleAllegiancePrism allegiance' = prism (set (role . allegiance) allegiance') $ \player ->
    if player ^. role . allegiance == allegiance' then Right player else Left player

-- | This 'Prism' provides the traversal of an 'Alive' state in a 'Player'.
alive :: Prism' Player Player
alive = genericStatePrism Alive

-- | This 'Prism' provides the traversal of a 'Dead' state in a 'Player'.
dead :: Prism' Player Player
dead = genericStatePrism Dead

genericStatePrism :: State -> Prism' Player Player
genericStatePrism state' = prism (set state state') $ \player ->
    if player ^. state == state' then Right player else Left player

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
-- 'angels' = 'traverse' . 'angel'
-- @
angels :: Traversable t => Traversal' (t Player) Player
angels = traverse . angel

-- | This 'Traversal' provides the traversal of 'bearTamer' 'Player's.
--
-- @
-- 'bearTamers' = 'traverse' . 'bearTamer'
-- @
bearTamers :: Traversable t => Traversal' (t Player) Player
bearTamers = traverse . bearTamer

-- | This 'Traversal' provides the traversal of 'defender' 'Player's.
--
-- @
-- 'defenders' = 'traverse' . 'defender'
-- @
defenders :: Traversable t => Traversal' (t Player) Player
defenders = traverse . defender

-- | This 'Traversal' provides the traversal of 'scapegoat' 'Player's.
--
-- @
-- 'scapegoats' = 'traverse' . 'scapegoat'
-- @
scapegoats :: Traversable t => Traversal' (t Player) Player
scapegoats = traverse . scapegoat

-- | This 'Traversal' provides the traversal of 'seer' 'Player's.
--
-- @
-- 'seers' = 'traverse' . 'seer'
-- @
seers :: Traversable t => Traversal' (t Player) Player
seers = traverse . seer

-- | This 'Traversal' provides the traversal of 'simpleVillager' 'Player's.
--
-- @
-- 'simpleVillagers' = 'traverse' . 'simpleVillager'
-- @
simpleVillagers :: Traversable t => Traversal' (t Player) Player
simpleVillagers = traverse . simpleVillager

-- | This 'Traversal' provides the traversal of 'simpleWerewolf' 'Player's.
--
-- @
-- 'simpleWerewolves' = 'traverse' . 'simpleWerewolf'
-- @
simpleWerewolves :: Traversable t => Traversal' (t Player) Player
simpleWerewolves = traverse . simpleWerewolf

-- | This 'Traversal' provides the traversal of 'villageIdiot' 'Player's.
--
-- @
-- 'villageIdiots' = 'traverse' . 'villageIdiot'
-- @
villageIdiots :: Traversable t => Traversal' (t Player) Player
villageIdiots = traverse . villageIdiot

-- | This 'Traversal' provides the traversal of 'villagerVillager' 'Player's.
--
-- @
-- 'villagerVillagers' = 'traverse' . 'villagerVillager'
-- @
villagerVillagers :: Traversable t => Traversal' (t Player) Player
villagerVillagers = traverse . villagerVillager

-- | This 'Traversal' provides the traversal of 'wildChild' 'Player's.
--
-- @
-- 'wildChildren' = 'traverse' . 'wildChild'
-- @
wildChildren :: Traversable t => Traversal' (t Player) Player
wildChildren = traverse . wildChild

-- | This 'Traversal' provides the traversal of 'witch' 'Player's.
--
-- @
-- 'witches' = 'traverse' . 'witch'
-- @
witches :: Traversable t => Traversal' (t Player) Player
witches = traverse . witch

-- | This 'Traversal' provides the traversal of 'wolfHound' 'Player's.
--
-- @
-- 'wolfHounds' = 'traverse' . 'wolfHound'
-- @
wolfHounds :: Traversable t => Traversal' (t Player) Player
wolfHounds = traverse . wolfHound

-- | This 'Traversal' provides the traversal of 'villager' 'Player's.
--
-- @
-- 'villagers' = 'traverse' . 'villager'
-- @
villagers :: Traversable t => Traversal' (t Player) Player
villagers = traverse . villager

-- | This 'Traversal' provides the traversal of 'werewolf' 'Player's.
--
-- @
-- 'werewolves' = 'traverse' . 'werewolf'
-- @
werewolves :: Traversable t => Traversal' (t Player) Player
werewolves = traverse . werewolf
