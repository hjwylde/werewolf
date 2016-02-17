{-|
Module      : Game.Werewolf
Description : Re-exports all of the public modules under /Game.Werewolf/.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Re-exports all of the public modules under /Game.Werewolf/.

Where clashes are found between "Game.Werewolf.Engine" and "Game.Werewolf.Game", the
"Game.Werewolf.Engine" functions are preferred.

Likewise, where clases are found between "Game.Werewolf.Player" and "Game.Werewolf.Role", the
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
import Game.Werewolf.Game     hiding (doesPlayerExist, getAllowedVoters, getDevourEvent,
                               getPendingVoters, getPlayerVote, getVoteResult, getVoteResult,
                               isDefendersTurn, isGameOver, isScapegoatsTurn, isSeersTurn,
                               isVillagesTurn, isWerewolvesTurn, isWildChildsTurn, isWitchsTurn,
                               isWolfHoundsTurn, killPlayer, setPlayerRole)
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role hiding (name)
