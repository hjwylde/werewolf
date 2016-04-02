{-|
Module      : Game.Werewolf.Util
Description : Utility functions for working in a ('MonadState' 'Game') environment.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Utility functions for woking in a ('MonadState' 'Game') environment.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

module Game.Werewolf.Util (
    -- * Game

    -- ** Manipulations
    killPlayer, removePlayer, setPlayerAllegiance, setPlayerRole,

    -- ** Searches
    findPlayerBy_, getAdjacentAlivePlayers, getPassers, getPlayerVote,
    getAllowedVoters, getPendingVoters, getVoteResult,

    -- ** Queries
    isDefendersTurn, isDevotedServantsTurn, isGameOver, isScapegoatsTurn, isSeersTurn, isSunrise,
    isVillagesTurn, isWerewolvesTurn, isWildChildsTurn, isWitchsTurn, isWolfHoundsTurn,
    hasAnyoneWon, hasAngelWon, hasVillagersWon, hasWerewolvesWon,

    -- * Player

    -- ** Queries
    doesPlayerExist,
    isPlayerDefender, isPlayerDevotedServant, isPlayerJester, isPlayerScapegoat, isPlayerSeer,
    isPlayerWildChild, isPlayerWitch, isPlayerWolfHound,
    isPlayerWerewolf,
    isPlayerAlive, isPlayerDead,
) where

import Control.Lens        hiding (cons)
import Control.Monad.Extra
import Control.Monad.State hiding (state)

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import           Game.Werewolf.Game   hiding (doesPlayerExist, getAllowedVoters, getPendingVoters,
                                       getVoteResult, hasAngelWon, hasAnyoneWon, hasVillagersWon,
                                       hasWerewolvesWon, killPlayer)
import qualified Game.Werewolf.Game   as Game
import           Game.Werewolf.Player
import           Game.Werewolf.Role   hiding (name)

import Prelude hiding (round)

killPlayer :: MonadState Game m => Text -> m ()
killPlayer name = modify $ Game.killPlayer name

removePlayer :: MonadState Game m => Text -> m ()
removePlayer name' = do
    killPlayer name'

    passes  %= delete name'
    votes   %= Map.delete name'

    player <- findPlayerBy_ name name'

    when (is angel player)      $ setPlayerAllegiance name' Villagers
    when (is defender player)   $ do
        protect         .= Nothing
        priorProtect    .= Nothing
    when (is seer player)       $ see .= Nothing
    when (is wildChild player)  $ roleModel .= Nothing
    when (is witch player)      $ do
        heal        .= False
        healUsed    .= False
        poison      .= Nothing
        poisonUsed  .= False
    when (is wolfHound player)  $ allegianceChosen .= Nothing

-- | Fudges the player's allegiance. This function is useful for roles such as the Wild-child where
--   they align themselves differently given some trigger.
setPlayerAllegiance :: MonadState Game m => Text -> Allegiance -> m ()
setPlayerAllegiance name' allegiance' = modify $ players . traverse . filteredBy name name' . role . allegiance .~ allegiance'

-- | Fudges the player's role. This function is useful for roles such as the Devoted Servant where
--   they take on a different role.
setPlayerRole :: MonadState Game m => Text -> Role -> m ()
setPlayerRole name' role' = modify $ players . traverse . filteredBy name name' . role .~ role'

findPlayerBy_ :: (Eq a, MonadState Game m) => Lens' Player a -> a -> m Player
findPlayerBy_ lens value = fromJust <$> preuse (players . traverse . filteredBy lens value)

getAdjacentAlivePlayers :: MonadState Game m => Text -> m [Player]
getAdjacentAlivePlayers name' = do
    alivePlayers    <- toListOf (players . traverse . alive) <$> get
    let index       = fromJust $ elemIndex name' (alivePlayers ^.. names)

    return $ adjacentElements index alivePlayers
    where
        adjacentElements 0 list     = last list : take 2 list
        adjacentElements index list = take 3 $ drop (index - 1) (cycle list)

getPassers :: MonadState Game m => m [Player]
getPassers = mapM (findPlayerBy_ name) =<< use passes

getPlayerVote :: MonadState Game m => Text -> m (Maybe Text)
getPlayerVote playerName = use $ votes . at playerName

getAllowedVoters :: MonadState Game m => m [Player]
getAllowedVoters = gets Game.getAllowedVoters

getPendingVoters :: MonadState Game m => m [Player]
getPendingVoters = gets Game.getPendingVoters

getVoteResult :: MonadState Game m => m [Player]
getVoteResult = gets Game.getVoteResult

isDefendersTurn :: MonadState Game m => m Bool
isDefendersTurn = has (stage . _DefendersTurn) <$> get

isDevotedServantsTurn :: MonadState Game m => m Bool
isDevotedServantsTurn = has (stage . _DevotedServantsTurn) <$> get

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

doesPlayerExist :: MonadState Game m => Text -> m Bool
doesPlayerExist name = gets $ Game.doesPlayerExist name

isPlayerDefender :: MonadState Game m => Text -> m Bool
isPlayerDefender name' = is defender <$> findPlayerBy_ name name'

isPlayerDevotedServant :: MonadState Game m => Text -> m Bool
isPlayerDevotedServant name' = is devotedServant <$> findPlayerBy_ name name'

isPlayerJester :: MonadState Game m => Text -> m Bool
isPlayerJester name' = is jester <$> findPlayerBy_ name name'

isPlayerScapegoat :: MonadState Game m => Text -> m Bool
isPlayerScapegoat name' = is scapegoat <$> findPlayerBy_ name name'

isPlayerSeer :: MonadState Game m => Text -> m Bool
isPlayerSeer name' = is seer <$> findPlayerBy_ name name'

isPlayerWildChild :: MonadState Game m => Text -> m Bool
isPlayerWildChild name' = is wildChild <$> findPlayerBy_ name name'

isPlayerWitch :: MonadState Game m => Text -> m Bool
isPlayerWitch name' = is witch <$> findPlayerBy_ name name'

isPlayerWolfHound :: MonadState Game m => Text -> m Bool
isPlayerWolfHound name' = is wolfHound <$> findPlayerBy_ name name'

isPlayerWerewolf :: MonadState Game m => Text -> m Bool
isPlayerWerewolf name' = is werewolf <$> findPlayerBy_ name name'

isPlayerAlive :: MonadState Game m => Text -> m Bool
isPlayerAlive name' = is alive <$> findPlayerBy_ name name'

isPlayerDead :: MonadState Game m => Text -> m Bool
isPlayerDead name' = is dead <$> findPlayerBy_ name name'
