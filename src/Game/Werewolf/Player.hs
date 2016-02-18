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
) where

import Game.Werewolf.Internal.Player
