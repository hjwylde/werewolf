{-|
Module      : Game.Werewolf.Internal.Util
Description : Utility functions for working in a ('MonadState' 'Game') environment.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Utility functions for woking in a ('MonadState' 'Game') environment.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Internal.Util (
    -- * Game

    -- ** Manipulations
    killPlayer, setPlayerAllegiance,

    -- ** Searches
    findPlayerByName_, findPlayerByRole_, getAdjacentAlivePlayers,

    -- ** Queries
    isDefendersTurn, isGameOver, isScapegoatsTurn, isSeersTurn, isSunrise, isVillagesTurn,
    isWerewolvesTurn, isWildChildsTurn, isWitchsTurn, isWolfHoundsTurn,
    hasAnyoneWon, hasAngelWon, hasVillagersWon, hasWerewolvesWon,
    getPassers, getPlayerVote, getAllowedVoters, getPendingVoters, getVoteResult,

    -- * Event

    -- ** Queries
    getDevourEvent,

    -- * Player

    -- ** Queries
    doesPlayerExist,
    isPlayerDefender, isPlayerScapegoat, isPlayerSeer, isPlayerVillageIdiot, isPlayerWildChild,
    isPlayerWitch, isPlayerWolfHound,
    isPlayerWerewolf,
    isPlayerAlive, isPlayerDead,
) where

import Control.Lens        hiding (cons)
import Control.Monad.Extra
import Control.Monad.State hiding (state)

import Data.List
import Data.Maybe
import Data.Text  (Text)

import           Game.Werewolf.Internal.Game   hiding (doesPlayerExist, getAllowedVoters,
                                                getPendingVoters, getVoteResult, hasAngelWon,
                                                hasVillagersWon, hasWerewolvesWon, killPlayer)
import qualified Game.Werewolf.Internal.Game   as Game
import           Game.Werewolf.Internal.Player
import           Game.Werewolf.Internal.Role   hiding (name)

import Prelude hiding (round)

killPlayer :: MonadState Game m => Text -> m ()
killPlayer name = modify $ Game.killPlayer name

-- | Fudges the player's allegiance. This function is useful for roles such as the Wild-child where
--   they align themselves differently given some trigger.
setPlayerAllegiance :: MonadState Game m => Text -> Allegiance -> m ()
setPlayerAllegiance name' allegiance' = modify $ players . traverse . filteredBy name name' . role . allegiance .~ allegiance'

findPlayerByName_ :: MonadState Game m => Text -> m Player
findPlayerByName_ name' = fromJust <$> preuse (players . traverse . filteredBy name name')

findPlayerByRole_ :: MonadState Game m => Role -> m Player
findPlayerByRole_ role' = fromJust <$> preuse (players . traverse . filteredBy role role')

getAdjacentAlivePlayers :: MonadState Game m => Text -> m [Player]
getAdjacentAlivePlayers name' = do
    alivePlayers    <- gets $ toListOf (players . traverse . alive)
    let index       = fromJust $ elemIndex name' (alivePlayers ^.. names)

    return $ adjacentElements index alivePlayers
    where
        adjacentElements 0 list     = last list : take 2 list
        adjacentElements index list = take 3 $ drop (index - 1) (cycle list)

isDefendersTurn :: MonadState Game m => m Bool
isDefendersTurn = has (stage . _DefendersTurn) <$> get

isGameOver :: MonadState Game m => m Bool
isGameOver = has (stage . _GameOver) <$> get

isScapegoatsTurn :: MonadState Game m => m Bool
isScapegoatsTurn = has (stage . _ScapegoatsTurn) <$> get

isSeersTurn :: MonadState Game m => m Bool
isSeersTurn = has (stage . _SeersTurn) <$> get

isSunrise :: MonadState Game m => m Bool
isSunrise = has (stage . _Sunrise) <$> get

isVillagesTurn :: MonadState Game m => m Bool
isVillagesTurn = has (stage . _VillagesTurn) <$> get

isWerewolvesTurn :: MonadState Game m => m Bool
isWerewolvesTurn = has (stage . _WerewolvesTurn) <$> get

isWildChildsTurn :: MonadState Game m => m Bool
isWildChildsTurn = has (stage . _WildChildsTurn) <$> get

isWitchsTurn :: MonadState Game m => m Bool
isWitchsTurn = has (stage . _WitchsTurn) <$> get

isWolfHoundsTurn :: MonadState Game m => m Bool
isWolfHoundsTurn = has (stage . _WolfHoundsTurn) <$> get

hasAnyoneWon :: MonadState Game m => m Bool
hasAnyoneWon = gets Game.hasAnyoneWon

hasAngelWon :: MonadState Game m => m Bool
hasAngelWon = gets Game.hasAngelWon

hasVillagersWon :: MonadState Game m => m Bool
hasVillagersWon = gets Game.hasVillagersWon

hasWerewolvesWon :: MonadState Game m => m Bool
hasWerewolvesWon = gets Game.hasWerewolvesWon

getPassers :: MonadState Game m => m [Player]
getPassers = gets $ \game -> map (\name' -> game ^?! players . traverse . filteredBy name name') (game ^. passes)

getPlayerVote :: MonadState Game m => Text -> m (Maybe Text)
getPlayerVote playerName = use $ votes . at playerName

getAllowedVoters :: MonadState Game m => m [Player]
getAllowedVoters = gets Game.getAllowedVoters

getPendingVoters :: MonadState Game m => m [Player]
getPendingVoters = gets Game.getPendingVoters

getVoteResult :: MonadState Game m => m [Player]
getVoteResult = gets Game.getVoteResult

getDevourEvent :: MonadState Game m => m (Maybe Text)
getDevourEvent = gets (^? events . traverse . _DevourEvent)

doesPlayerExist :: MonadState Game m => Text -> m Bool
doesPlayerExist name = gets $ Game.doesPlayerExist name

isPlayerDefender :: MonadState Game m => Text -> m Bool
isPlayerDefender name = is defender <$> findPlayerByName_ name

isPlayerScapegoat :: MonadState Game m => Text -> m Bool
isPlayerScapegoat name = is scapegoat <$> findPlayerByName_ name

isPlayerSeer :: MonadState Game m => Text -> m Bool
isPlayerSeer name = is seer <$> findPlayerByName_ name

isPlayerVillageIdiot :: MonadState Game m => Text -> m Bool
isPlayerVillageIdiot name = is villageIdiot <$> findPlayerByName_ name

isPlayerWildChild :: MonadState Game m => Text -> m Bool
isPlayerWildChild name = is wildChild <$> findPlayerByName_ name

isPlayerWitch :: MonadState Game m => Text -> m Bool
isPlayerWitch name = is witch <$> findPlayerByName_ name

isPlayerWolfHound :: MonadState Game m => Text -> m Bool
isPlayerWolfHound name = is wolfHound <$> findPlayerByName_ name

isPlayerWerewolf :: MonadState Game m => Text -> m Bool
isPlayerWerewolf name = is werewolf <$> findPlayerByName_ name

isPlayerAlive :: MonadState Game m => Text -> m Bool
isPlayerAlive name = is alive <$> findPlayerByName_ name

isPlayerDead :: MonadState Game m => Text -> m Bool
isPlayerDead name = is dead <$> findPlayerByName_ name
