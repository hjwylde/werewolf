{-|
Module      : Game.Werewolf.Player
Description : Simplistic player data structure.
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Player functions are defined in "Game.Werewolf.Internal.Player". This module just re-exports the
functions relevant to the public interface.
-}

module Game.Werewolf.Player (
    -- * Player
    Player, name, role, state,

    State(..),

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
) where

import Game.Werewolf.Internal.Player
