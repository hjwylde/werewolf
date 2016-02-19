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
    Role, name, allegiance, balance, description, advice,

    Allegiance(..),

    -- ** Prisms
    -- | N.B., these are not legal traversals for the same reason 'filtered' isn't!
    angel, villager, werewolf,

    -- ** Instances
    allRoles, restrictedRoles,
) where

import Game.Werewolf.Internal.Role
