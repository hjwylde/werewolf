{-|
Module      : Game.Werewolf.Game
Description : Game data structure.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Game functions are defined in "Game.Werewolf.Internal.Game". This module just re-exports the
functions relevant to the public interface.
-}

module Game.Werewolf.Game (
    -- * Game
    Game, stage, round, players, events, passes, allowedVoters, heal, healUsed, poison, poisonUsed,
    priorProtect, protect, roleModel, scapegoatBlamed, see, villageIdiotRevealed, votes,

    Stage(..),

    Event(..),

    -- ** Prisms
    -- | N.B., these are not legal traversals for the same reason 'filtered' isn't!
    defendersTurn, gameOver, scapegoatsTurn, seersTurn, sunrise, sunset, ursussGrunt, villagesTurn,
    werewolvesTurn, wildChildsTurn, witchsTurn, wolfHoundsTurn,

    -- ** Queries
    doesPlayerExist,
) where

import Game.Werewolf.Internal.Game

import Prelude hiding (round)
