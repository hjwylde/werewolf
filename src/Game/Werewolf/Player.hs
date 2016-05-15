{-|
Module      : Game.Werewolf.Player
Description : Simplistic player data structure with lenses for searching, filtering and querying
              lists of players.
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Players are quite simple in themselves. They have a 'name', 'role' and 'state'.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Game.Werewolf.Player (
    -- * Player
    Player,
    name, role, state,

    State(..),
    _Alive, _Dead,

    newPlayer,

    -- ** Traversals
    alphaWolf, beholder, crookedSenator, druid, fallenAngel, hunter, jester, lycan, medusa, oracle,
    orphan, protector, scapegoat, seer, simpleVillager, simpleWerewolf, spitefulGhost, trueVillager,
    villageDrunk, witch,
    loner, villager, werewolf,

    -- | The following traversals are provided just as a bit of sugar to avoid continually writing
    --   @'traverse' .@.
    names, roles, states,

    -- | N.B., the following traversals are not legal for the same reason 'filtered' isn't!
    named,
    alphaWolves, beholders, crookedSenators, druids, fallenAngels, hunters, jesters, lycans,
    medusas, oracles, orphans, protectors, scapegoats, seers, simpleVillagers, simpleWerewolves,
    spitefulGhosts, trueVillagers, villageDrunks, witches,
    loners, villagers, werewolves,
    alive, dead,
) where

import Control.Lens       hiding (isn't)
import Control.Lens.Extra

import Data.Function
import Data.String.Humanise
import Data.Text            as T

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

instance Humanise Player where
    humanise = view name

makePrisms ''State

-- | Creates a new 'Alive' player, with one exception: a 'spitefulGhost' always starts 'Dead'.
newPlayer :: Text -> Role -> Player
newPlayer name role = Player name role state
    where
        state = if role == spitefulGhostRole then Dead else Alive

-- | The traversal of 'Player's with an 'alphaWolfRole'.
--
-- @
-- 'alphaWolf' = 'role' . 'only' 'alphaWolfRole'
-- @
alphaWolf :: Traversal' Player ()
alphaWolf = role . only alphaWolfRole

-- | The traversal of 'Player's with a 'beholderRole'.
--
-- @
-- 'beholder' = 'role' . 'only' 'beholderRole'
-- @
beholder :: Traversal' Player ()
beholder = role . only beholderRole

-- | The traversal of 'Player's with a 'crookedSenatorRole'.
--
-- @
-- 'crookedSenator' = 'role' . 'only' 'crookedSenatorRole'
-- @
crookedSenator :: Traversal' Player ()
crookedSenator = role . only crookedSenatorRole

-- | The traversal of 'Player's with a 'druidRole'.
--
-- @
-- 'druid' = 'role' . 'only' 'druidRole'
-- @
druid :: Traversal' Player ()
druid = role . only druidRole

-- | The traversal of 'Player's with a 'fallenAngelRole'.
--
-- @
-- 'fallenAngel' = 'role' . 'only' 'fallenAngelRole'
-- @
fallenAngel :: Traversal' Player ()
fallenAngel = role . only fallenAngelRole

-- | The traversal of 'Player's with a 'hunterRole'.
--
-- @
-- 'hunter' = 'role' . 'only' 'hunterRole'
-- @
hunter :: Traversal' Player ()
hunter = role . only hunterRole

-- | The traversal of 'Player's with a 'jesterRole'.
--
-- @
-- 'jester' = 'role' . 'only' 'jesterRole'
-- @
jester :: Traversal' Player ()
jester = role . only jesterRole

-- | The traversal of 'Player's with a 'lycanRole'.
--
-- @
-- 'lycan' = 'role' . 'only' 'lycanRole'
-- @
lycan :: Traversal' Player ()
lycan = role . only lycanRole

-- | The traversal of 'Player's with a 'medusaRole'.
--
-- @
-- 'medusa' = 'role' . 'only' 'medusaRole'
-- @
medusa :: Traversal' Player ()
medusa = role . only medusaRole

-- | The traversal of 'Player's with a 'oracleRole'.
--
-- @
-- 'oracle' = 'role' . 'only' 'oracleRole'
-- @
oracle :: Traversal' Player ()
oracle = role . only oracleRole

-- | The traversal of 'Player's with an 'orphanRole'.
--
-- @
-- 'orphan' = 'role' . 'only' 'orphanRole'
-- @
orphan :: Traversal' Player ()
orphan = role . only orphanRole

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

-- | The traversal of 'Player's with a 'spitefulGhostRole'.
--
-- @
-- 'spitefulGhost' = 'role' . 'only' 'spitefulGhostRole'
-- @
spitefulGhost :: Traversal' Player ()
spitefulGhost = role . only spitefulGhostRole

-- | The traversal of 'Player's with a 'trueVillagerRole'.
--
-- @
-- 'trueVillager' = 'role' . 'only' 'trueVillagerRole'
-- @
trueVillager :: Traversal' Player ()
trueVillager = role . only trueVillagerRole

-- | The traversal of 'Player's with a 'villageDrunkRole'.
--
-- @
-- 'villageDrunk' = 'role' . 'only' 'villageDrunkRole'
-- @
villageDrunk :: Traversal' Player ()
villageDrunk = role . only villageDrunkRole

-- | The traversal of 'Player's with a 'witchRole'.
--
-- @
-- 'witch' = 'role' . 'only' 'witchRole'
-- @
witch :: Traversal' Player ()
witch = role . only witchRole

-- | The traversal of 'Player's aligned with 'NoOne'.
--
-- @
-- 'loner' = 'role' . 'allegiance' . '_NoOne'
-- @
loner :: Traversal' Player ()
loner = role . allegiance . _NoOne

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

-- | This 'Traversal' provides the traversal of 'Player's with the given name.
--
-- @
-- 'named' name' = 'filteredBy' . 'name' name'
-- @
named :: Text -> Traversal' Player Player
named name' = filteredBy name name'

-- | This 'Traversal' provides the traversal of 'alphaWolf' 'Player's.
--
-- @
-- 'alphaWolves' = 'traverse' . 'filtered' ('is' 'alphaWolf')
-- @
alphaWolves :: Traversable t => Traversal' (t Player) Player
alphaWolves = traverse . filtered (is alphaWolf)

-- | This 'Traversal' provides the traversal of 'beholder' 'Player's.
--
-- @
-- 'beholders' = 'traverse' . 'filtered' ('is' 'beholder')
-- @
beholders :: Traversable t => Traversal' (t Player) Player
beholders = traverse . filtered (is beholder)

-- | This 'Traversal' provides the traversal of 'crookedSenator' 'Player's.
--
-- @
-- 'crookedSenators' = 'traverse' . 'filtered' ('is' 'crookedSenator')
-- @
crookedSenators :: Traversable t => Traversal' (t Player) Player
crookedSenators = traverse . filtered (is crookedSenator)

-- | This 'Traversal' provides the traversal of 'druid' 'Player's.
--
-- @
-- 'druids' = 'traverse' . 'filtered' ('is' 'druid')
-- @
druids :: Traversable t => Traversal' (t Player) Player
druids = traverse . filtered (is druid)

-- | This 'Traversal' provides the traversal of 'fallenAngel' 'Player's.
--
-- @
-- 'fallenAngels' = 'traverse' . 'filtered' ('is' 'fallenAngel')
-- @
fallenAngels :: Traversable t => Traversal' (t Player) Player
fallenAngels = traverse . filtered (is fallenAngel)

-- | This 'Traversal' provides the traversal of 'hunter' 'Player's.
--
-- @
-- 'hunters' = 'traverse' . 'filtered' ('is' 'hunter')
-- @
hunters :: Traversable t => Traversal' (t Player) Player
hunters = traverse . filtered (is hunter)

-- | This 'Traversal' provides the traversal of 'jester' 'Player's.
--
-- @
-- 'jesters' = 'traverse' . 'filtered' ('is' 'jester')
-- @
jesters :: Traversable t => Traversal' (t Player) Player
jesters = traverse . filtered (is jester)

-- | This 'Traversal' provides the traversal of 'lycan' 'Player's.
--
-- @
-- 'lycans' = 'traverse' . 'filtered' ('is' 'lycan')
-- @
lycans :: Traversable t => Traversal' (t Player) Player
lycans = traverse . filtered (is lycan)

-- | This 'Traversal' provides the traversal of 'medusa' 'Player's.
--
-- @
-- 'medusas' = 'traverse' . 'filtered' ('is' 'medusa')
-- @
medusas :: Traversable t => Traversal' (t Player) Player
medusas = traverse . filtered (is medusa)

-- | This 'Traversal' provides the traversal of 'oracle' 'Player's.
--
-- @
-- 'oracles' = 'traverse' . 'filtered' ('is' 'oracle')
-- @
oracles :: Traversable t => Traversal' (t Player) Player
oracles = traverse . filtered (is oracle)

-- | This 'Traversal' provides the traversal of 'orphan' 'Player's.
--
-- @
-- 'orphans' = 'traverse' . 'filtered' ('is' 'orphan')
-- @
orphans :: Traversable t => Traversal' (t Player) Player
orphans = traverse . filtered (is orphan)

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

-- | This 'Traversal' provides the traversal of 'spitefulGhost' 'Player's.
--
-- @
-- 'spitefulGhosts' = 'traverse' . 'filtered' ('is' 'spitefulGhost')
-- @
spitefulGhosts :: Traversable t => Traversal' (t Player) Player
spitefulGhosts = traverse . filtered (is spitefulGhost)

-- | This 'Traversal' provides the traversal of 'trueVillager' 'Player's.
--
-- @
-- 'trueVillagers' = 'traverse' . 'filtered' ('is' 'trueVillager')
-- @
trueVillagers :: Traversable t => Traversal' (t Player) Player
trueVillagers = traverse . filtered (is trueVillager)

-- | This 'Traversal' provides the traversal of 'villageDrunk' 'Player's.
--
-- @
-- 'villageDrunks' = 'traverse' . 'filtered' ('is' 'villageDrunk')
-- @
villageDrunks :: Traversable t => Traversal' (t Player) Player
villageDrunks = traverse . filtered (is villageDrunk)

-- | This 'Traversal' provides the traversal of 'witch' 'Player's.
--
-- @
-- 'witches' = 'traverse' . 'filtered' ('is' 'witch')
-- @
witches :: Traversable t => Traversal' (t Player) Player
witches = traverse . filtered (is witch)

-- | This 'Traversal' provides the traversal of 'loner' 'Player's.
--
-- @
-- 'loners' = 'traverse' . 'filtered' . ('is' 'loner')
-- @
loners :: Traversable t => Traversal' (t Player) Player
loners = traverse . filtered (is loner)

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
