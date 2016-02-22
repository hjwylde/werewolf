{-|
Module      : Game.Werewolf.Role
Description : Simplistic role data structure and instances.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Role instances are defined in "Game.Werewolf.Internal.Role". This module just re-exports the
functions relevant to the public interface.

The roles are split into four categories:

* The Ambiguous.
* The Loners.
* The Villagers.
* The Werewolves.
-}

module Game.Werewolf.Role (
    -- * Role
    Role,
    name, allegiance, balance, description, advice,

    Allegiance(..),
    _Angel, _Villagers, _Werewolves,

    -- ** Instances
    allRoles, restrictedRoles,

    -- *** The Ambiguous
    -- | The Ambiguous may change allegiance during the game.
    wildChildRole, wolfHoundRole,

    -- *** The Loners
    -- | The Loners have their own win condition.
    angelRole,

    -- *** The Villagers
    -- | The Villagers must lynch all of the Werewolves.
    bearTamerRole, defenderRole, scapegoatRole, seerRole, simpleVillagerRole, villageIdiotRole,
    villagerVillagerRole, witchRole,

    -- *** The Werewolves
    -- | The Werewolves must devour all of the Villagers.
    simpleWerewolfRole,

) where

import Game.Werewolf.Internal.Role
