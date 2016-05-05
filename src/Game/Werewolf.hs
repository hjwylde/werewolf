{-|
Module      : Game.Werewolf
Description : Re-exports all of the public modules under /Game.Werewolf/.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Re-exports all of the public modules under /Game.Werewolf/. These are:

* "Game.Werewolf.Game"
* "Game.Werewolf.Player"
* "Game.Werewolf.Response"
* "Game.Werewolf.Role"

N.B., where clashes are found between "Game.Werewolf.Player" and "Game.Werewolf.Role", the
"Game.Werewolf.Player" functions are preferred.
-}

module Game.Werewolf (
    module Werewolf
) where

import Game.Werewolf.Game     as Werewolf
import Game.Werewolf.Player   as Werewolf
import Game.Werewolf.Response as Werewolf
import Game.Werewolf.Role     as Werewolf hiding (name)
