{-|
Module      : Game.Werewolf
Description : Re-exports all of the public modules under /Game.Werewolf/.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Re-exports all of the public modules under /Game.Werewolf/.

Likewise, where clashes are found between "Game.Werewolf.Player" and "Game.Werewolf.Role", the
"Game.Werewolf.Player" functions are preferred.
-}

module Game.Werewolf (
    module Game.Werewolf.Command,
    module Game.Werewolf.Engine,
    module Game.Werewolf.Game,
    module Game.Werewolf.Player,
    module Game.Werewolf.Response,
    module Game.Werewolf.Role
) where

import Game.Werewolf.Command
import Game.Werewolf.Engine
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role     hiding (name)
