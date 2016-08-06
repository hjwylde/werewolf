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
    alphaWolf, beholder, crookedSenator, druid, dullahan, fallenAngel, hunter, jester, lycan,
    medusa, necromancer, oracle, orphan, protector, saint, scapegoat, seer, simpleVillager,
    simpleWerewolf, spitefulVillager, trueVillager, villageDrunk, witch, zombie,
    loner, villager, werewolf,

    -- | The following traversals are provided just as a bit of sugar to avoid continually writing
    --   @'traverse' .@.
    names, roles, states,

    -- | N.B., the following traversals are not legal for the same reason 'filtered' isn't!
    named,
    alphaWolves, beholders, crookedSenators, druids, dullahans, fallenAngels, hunters, jesters,
    lycans, medusas, necromancers, oracles, orphans, protectors, saints, scapegoats, seers,
    simpleVillagers, simpleWerewolves, spitefulVillagers, trueVillagers, villageDrunks, witches,
    zombies,
    loners, villagers, werewolves,
    alive, dead,
) where

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

-- | Creates a new 'Alive' player.
newPlayer :: Text -> Role -> Player
newPlayer name role = Player name role Alive

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

-- | The traversal of 'Player's with a 'dullahanRole'.
--
-- @
-- 'dullahan' = 'role' . 'only' 'dullahanRole'
-- @
dullahan :: Traversal' Player ()
dullahan = role . only dullahanRole

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

-- | The traversal of 'Player's with a 'necromancerRole'.
--
-- @
-- 'necromancer' = 'role' . 'only' 'necromancerRole'
-- @
necromancer :: Traversal' Player ()
necromancer = role . only necromancerRole

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

-- | The traversal of 'Player's with a 'saintRole'.
--
-- @
-- 'saint' = 'role' . 'only' 'saintRole'
-- @
saint :: Traversal' Player ()
saint = role . only saintRole

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

-- | The traversal of 'Player's with a 'spitefulVillagerRole'.
--
-- @
-- 'spitefulVillager' = 'role' . 'only' 'spitefulVillagerRole'
-- @
spitefulVillager :: Traversal' Player ()
spitefulVillager = role . only spitefulVillagerRole

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

-- | The traversal of 'Player's with a 'zombieRole'.
--
-- @
-- 'zombie' = 'role' . 'only' 'zombieRole'
-- @
zombie :: Traversal' Player ()
zombie = role . only zombieRole

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

-- | The traversal of 'Player' names.
--
-- @
-- 'names' = 'traverse' . 'name'
-- @
names :: Traversable t => Traversal' (t Player) Text
names = traverse . name

-- | The traversal of 'Player' roles.
--
-- @
-- 'roles' = 'traverse' . 'role'
-- @
roles :: Traversable t => Traversal' (t Player) Role
roles = traverse . role

-- | The traversal of 'Player' states.
--
-- @
-- 'states' = 'traverse' . 'state'
-- @
states :: Traversable t => Traversal' (t Player) State
states = traverse . state

-- | The traversal of 'Player's with the given name.
--
-- @
-- 'named' name' = 'filteredBy' . 'name' name'
-- @
named :: Text -> Traversal' Player Player
named name' = filteredBy name name'

-- | The traversal of 'alphaWolf' 'Player's.
--
-- @
-- 'alphaWolves' = 'traverse' . 'filtered' ('is' 'alphaWolf')
-- @
alphaWolves :: Traversable t => Traversal' (t Player) Player
alphaWolves = traverse . filtered (is alphaWolf)

-- | The traversal of 'beholder' 'Player's.
--
-- @
-- 'beholders' = 'traverse' . 'filtered' ('is' 'beholder')
-- @
beholders :: Traversable t => Traversal' (t Player) Player
beholders = traverse . filtered (is beholder)

-- | The traversal of 'crookedSenator' 'Player's.
--
-- @
-- 'crookedSenators' = 'traverse' . 'filtered' ('is' 'crookedSenator')
-- @
crookedSenators :: Traversable t => Traversal' (t Player) Player
crookedSenators = traverse . filtered (is crookedSenator)

-- | The traversal of 'dullahan' 'Player's.
--
-- @
-- 'dullahans' = 'traverse' . 'filtered' ('is' 'dullahan')
-- @
dullahans :: Traversable t => Traversal' (t Player) Player
dullahans = traverse . filtered (is dullahan)

-- | The traversal of 'druid' 'Player's.
--
-- @
-- 'druids' = 'traverse' . 'filtered' ('is' 'druid')
-- @
druids :: Traversable t => Traversal' (t Player) Player
druids = traverse . filtered (is druid)

-- | The traversal of 'fallenAngel' 'Player's.
--
-- @
-- 'fallenAngels' = 'traverse' . 'filtered' ('is' 'fallenAngel')
-- @
fallenAngels :: Traversable t => Traversal' (t Player) Player
fallenAngels = traverse . filtered (is fallenAngel)

-- | The traversal of 'hunter' 'Player's.
--
-- @
-- 'hunters' = 'traverse' . 'filtered' ('is' 'hunter')
-- @
hunters :: Traversable t => Traversal' (t Player) Player
hunters = traverse . filtered (is hunter)

-- | The traversal of 'jester' 'Player's.
--
-- @
-- 'jesters' = 'traverse' . 'filtered' ('is' 'jester')
-- @
jesters :: Traversable t => Traversal' (t Player) Player
jesters = traverse . filtered (is jester)

-- | The traversal of 'lycan' 'Player's.
--
-- @
-- 'lycans' = 'traverse' . 'filtered' ('is' 'lycan')
-- @
lycans :: Traversable t => Traversal' (t Player) Player
lycans = traverse . filtered (is lycan)

-- | The traversal of 'medusa' 'Player's.
--
-- @
-- 'medusas' = 'traverse' . 'filtered' ('is' 'medusa')
-- @
medusas :: Traversable t => Traversal' (t Player) Player
medusas = traverse . filtered (is medusa)

-- | The traversal of 'necromancer' 'Player's.
--
-- @
-- 'necromancers' = 'traverse' . 'filtered' ('is' 'necromancer')
-- @
necromancers :: Traversable t => Traversal' (t Player) Player
necromancers = traverse . filtered (is necromancer)

-- | The traversal of 'oracle' 'Player's.
--
-- @
-- 'oracles' = 'traverse' . 'filtered' ('is' 'oracle')
-- @
oracles :: Traversable t => Traversal' (t Player) Player
oracles = traverse . filtered (is oracle)

-- | The traversal of 'orphan' 'Player's.
--
-- @
-- 'orphans' = 'traverse' . 'filtered' ('is' 'orphan')
-- @
orphans :: Traversable t => Traversal' (t Player) Player
orphans = traverse . filtered (is orphan)

-- | The traversal of 'protector' 'Player's.
--
-- @
-- 'protectors' = 'traverse' . 'filtered' ('is' 'protector')
-- @
protectors :: Traversable t => Traversal' (t Player) Player
protectors = traverse . filtered (is protector)

-- | The traversal of 'saint' 'Player's.
--
-- @
-- 'saints' = 'traverse' . 'filtered' ('is' 'saint')
-- @
saints :: Traversable t => Traversal' (t Player) Player
saints = traverse . filtered (is saint)

-- | The traversal of 'scapegoat' 'Player's.
--
-- @
-- 'scapegoats' = 'traverse' . 'filtered' ('is' 'scapegoat')
-- @
scapegoats :: Traversable t => Traversal' (t Player) Player
scapegoats = traverse . filtered (is scapegoat)

-- | The traversal of 'seer' 'Player's.
--
-- @
-- 'seers' = 'traverse' . 'filtered' ('is' 'seer')
-- @
seers :: Traversable t => Traversal' (t Player) Player
seers = traverse . filtered (is seer)

-- | The traversal of 'simpleVillager' 'Player's.
--
-- @
-- 'simpleVillagers' = 'traverse' . 'filtered' ('is' 'simpleVillager')
-- @
simpleVillagers :: Traversable t => Traversal' (t Player) Player
simpleVillagers = traverse . filtered (is simpleVillager)

-- | The traversal of 'simpleWerewolf' 'Player's.
--
-- @
-- 'simpleWerewolves' = 'traverse' . 'filtered' ('is' 'simpleWerewolf')
-- @
simpleWerewolves :: Traversable t => Traversal' (t Player) Player
simpleWerewolves = traverse . filtered (is simpleWerewolf)

-- | The traversal of 'spitefulVillager' 'Player's.
--
-- @
-- 'spitefulVillagers' = 'traverse' . 'filtered' ('is' 'spitefulVillager')
-- @
spitefulVillagers :: Traversable t => Traversal' (t Player) Player
spitefulVillagers = traverse . filtered (is spitefulVillager)

-- | The traversal of 'trueVillager' 'Player's.
--
-- @
-- 'trueVillagers' = 'traverse' . 'filtered' ('is' 'trueVillager')
-- @
trueVillagers :: Traversable t => Traversal' (t Player) Player
trueVillagers = traverse . filtered (is trueVillager)

-- | The traversal of 'villageDrunk' 'Player's.
--
-- @
-- 'villageDrunks' = 'traverse' . 'filtered' ('is' 'villageDrunk')
-- @
villageDrunks :: Traversable t => Traversal' (t Player) Player
villageDrunks = traverse . filtered (is villageDrunk)

-- | The traversal of 'witch' 'Player's.
--
-- @
-- 'witches' = 'traverse' . 'filtered' ('is' 'witch')
-- @
witches :: Traversable t => Traversal' (t Player) Player
witches = traverse . filtered (is witch)

-- | The traversal of 'zombie' 'Player's.
--
-- @
-- 'zombies' = 'traverse' . 'filtered' ('is' 'zombie')
-- @
zombies :: Traversable t => Traversal' (t Player) Player
zombies = traverse . filtered (is zombie)

-- | The traversal of 'loner' 'Player's.
--
-- @
-- 'loners' = 'traverse' . 'filtered' . ('is' 'loner')
-- @
loners :: Traversable t => Traversal' (t Player) Player
loners = traverse . filtered (is loner)

-- | The traversal of 'villager' 'Player's.
--
-- @
-- 'villagers' = 'traverse' . 'filtered' ('is' 'villager')
-- @
villagers :: Traversable t => Traversal' (t Player) Player
villagers = traverse . filtered (is villager)

-- | The traversal of 'werewolf' 'Player's.
--
-- @
-- 'werewolves' = 'traverse' . 'filtered' ('is' 'werewolf')
-- @
werewolves :: Traversable t => Traversal' (t Player) Player
werewolves = traverse . filtered (is werewolf)

-- | The traversal of 'Alive' 'Player's.
--
-- @
-- 'alive' = 'filtered' ('has' $ 'state' . '_Alive')
-- @
alive :: Traversal' Player Player
alive = filtered (has $ state . _Alive)

-- | The traversal of 'Dead' 'Player's.
--
-- @
-- 'dead' = 'filtered' ('has' $ 'state' . '_Dead')
-- @
dead :: Traversal' Player Player
dead = filtered (has $ state . _Dead)
