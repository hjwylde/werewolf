{-|
Module      : Game.Werewolf.Role
Description : Simplistic role data structure and instances.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Role instances are defined in "Game.Werewolf.Internal.Role". This module just re-exports the
functions relevant to the public interface.
-}

module Game.Werewolf.Role (
    -- * Role
    Role,
    name, allegiance, balance, description, advice,

    Allegiance(..),
    _Angel, _Villagers, _Werewolves,

    -- ** Instances
    allRoles, restrictedRoles,
) where

import Game.Werewolf.Internal.Role
