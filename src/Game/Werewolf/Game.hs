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
    Game,
    stage, round, players, events, passes, allegianceChosen, allowedVoters, heal, healUsed, poison,
    poisonUsed, priorProtect, protect, roleModel, scapegoatBlamed, see, villageIdiotRevealed, votes,

    Stage(..),
    _DefendersTurn, _GameOver, _ScapegoatsTurn, _SeersTurn, _Sunrise, _Sunset, _UrsussGrunt,
    _VillagesTurn, _WerewolvesTurn, _WildChildsTurn, _WitchsTurn, _WolfHoundsTurn,

    Event(..),
    _DevourEvent, _NoDevourEvent, _PoisonEvent,

    -- ** Queries
    doesPlayerExist,
) where

import Game.Werewolf.Internal.Game

import Prelude hiding (round)
