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
    killPlayer, removePlayer, setPlayerAllegiance, getRandomAllegiance,

    -- ** Searches
    findPlayerBy_, getAdjacentAlivePlayers, getFirstAdjacentAliveWerewolf, getPlayerVote,
    getAllowedVoters, getPendingVoters,

    -- ** Queries
    isGameOver, isHuntersTurn, isOraclesTurn, isOrphansTurn, isProtectorsTurn, isScapegoatsTurn,
    isSeersTurn, isSunrise, isVillagesTurn, isWerewolvesTurn, isWitchsTurn,
    hasAnyoneWon, hasVillagersWon, hasWerewolvesWon, hasEveryoneLost,

    -- * Player

    -- ** Queries
    doesPlayerExist,
    isPlayerDullahan, isPlayerHunter, isPlayerJester, isPlayerOracle, isPlayerOrphan,
    isPlayerProtector, isPlayerScapegoat, isPlayerSeer, isPlayerWitch,
    isPlayerWerewolf,
    isPlayerAlive, isPlayerDead,
) where

import Control.Lens.Extra
import Control.Monad.Extra
import Control.Monad.Random
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import           Game.Werewolf.Game           hiding (getAllowedVoters, getPendingVoters,
                                               hasAnyoneWon, hasEveryoneLost, hasVillagersWon,
                                               hasWerewolvesWon)
import qualified Game.Werewolf.Game           as Game
import           Game.Werewolf.Message.Engine
import           Game.Werewolf.Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role           hiding (name)

import Prelude hiding (round)

killPlayer :: (MonadState Game m, MonadWriter [Message] m) => Text -> m ()
killPlayer name = do
    tell [playerKilledMessage name]

    players . traverse . named name . state .= Dead

    whenM (isPlayerSpitefulVillager name) $ tell . (:[]) . spitefulVillagerKilledMessage name =<< get

removePlayer :: (MonadState Game m, MonadWriter [Message] m) => Text -> m ()
removePlayer name' = do
    killPlayer name'

    votes %= Map.delete name'

    player <- findPlayerBy_ name name'

    when (is orphan player)         $ roleModel .= Nothing
    when (is protector player)      $ do
        protect         .= Nothing
        priorProtect    .= Nothing
    when (is seer player)           $ see .= Nothing
    when (is witch player)          $ do
        healUsed    .= False
        poison      .= Nothing
        poisonUsed  .= False

-- | Fudges the player's allegiance. This function is useful for roles such as the Orphan where
--   they align themselves differently given some trigger.
setPlayerAllegiance :: MonadState Game m => Text -> Allegiance -> m ()
setPlayerAllegiance name allegiance' = modify $ players . traverse . named name . role . allegiance .~ allegiance'

-- | Get a random allegiance (either Villagers or Werewolves).
getRandomAllegiance :: MonadRandom m => m Allegiance
getRandomAllegiance = fromList [(Villagers, 0.5), (Werewolves, 0.5)]

findPlayerBy_ :: (Eq a, MonadState Game m) => Lens' Player a -> a -> m Player
findPlayerBy_ lens value = fromJust <$> preuse (players . traverse . filteredBy lens value)

getAdjacentAlivePlayers :: MonadState Game m => Text -> m [Player]
getAdjacentAlivePlayers name' = do
    alivePlayers    <- toListOf (players . traverse . alive) <$> get
    let index       = fromJust $ elemIndex name' (alivePlayers ^.. names)

    return $ adjacentElements index alivePlayers
    where
        adjacentElements 0 list     = last list : take 2 list
        adjacentElements index list = take 3 . drop (index - 1) $ cycle list

getFirstAdjacentAliveWerewolf :: MonadState Game m => Text -> m (Maybe Player)
getFirstAdjacentAliveWerewolf name = do
    players'            <- toListOf (players . traverse) <$> get
    let index           = fromJust $ elemIndex name (players' ^.. names)
    let filteredPlayers = dropWhile (isn't werewolf) (drop index (players' ++ players') ^.. traverse . alive)

    return $ listToMaybe filteredPlayers

getPlayerVote :: MonadState Game m => Text -> m (Maybe Text)
getPlayerVote playerName = use $ votes . at playerName

getAllowedVoters :: MonadState Game m => m [Player]
getAllowedVoters = gets Game.getAllowedVoters

getPendingVoters :: MonadState Game m => m [Player]
getPendingVoters = gets Game.getPendingVoters

isGameOver :: MonadState Game m => m Bool
isGameOver = hasuse $ stage . _GameOver

isHuntersTurn :: MonadState Game m => m Bool
isHuntersTurn = orM
    [ hasuse $ stage . _HuntersTurn1
    , hasuse $ stage . _HuntersTurn2
    ]

isOraclesTurn :: MonadState Game m => m Bool
isOraclesTurn = hasuse $ stage . _OraclesTurn

isOrphansTurn :: MonadState Game m => m Bool
isOrphansTurn = hasuse $ stage . _OrphansTurn

isProtectorsTurn :: MonadState Game m => m Bool
isProtectorsTurn = hasuse $ stage . _ProtectorsTurn

isScapegoatsTurn :: MonadState Game m => m Bool
isScapegoatsTurn = hasuse $ stage . _ScapegoatsTurn

isSeersTurn :: MonadState Game m => m Bool
isSeersTurn = hasuse $ stage . _SeersTurn

isSunrise :: MonadState Game m => m Bool
isSunrise = hasuse $ stage . _Sunrise

isVillagesTurn :: MonadState Game m => m Bool
isVillagesTurn = hasuse $ stage . _VillagesTurn

isWerewolvesTurn :: MonadState Game m => m Bool
isWerewolvesTurn = hasuse $ stage . _WerewolvesTurn

isWitchsTurn :: MonadState Game m => m Bool
isWitchsTurn = hasuse $ stage . _WitchsTurn

hasAnyoneWon :: MonadState Game m => m Bool
hasAnyoneWon = gets Game.hasAnyoneWon

hasVillagersWon :: MonadState Game m => m Bool
hasVillagersWon = gets Game.hasVillagersWon

hasWerewolvesWon :: MonadState Game m => m Bool
hasWerewolvesWon = gets Game.hasWerewolvesWon

hasEveryoneLost :: MonadState Game m => m Bool
hasEveryoneLost = gets Game.hasEveryoneLost

doesPlayerExist :: MonadState Game m => Text -> m Bool
doesPlayerExist name = hasuse $ players . traverse . named name

isPlayerDullahan :: MonadState Game m => Text -> m Bool
isPlayerDullahan name' = is dullahan <$> findPlayerBy_ name name'

isPlayerHunter :: MonadState Game m => Text -> m Bool
isPlayerHunter name' = is hunter <$> findPlayerBy_ name name'

isPlayerJester :: MonadState Game m => Text -> m Bool
isPlayerJester name' = is jester <$> findPlayerBy_ name name'

isPlayerOracle :: MonadState Game m => Text -> m Bool
isPlayerOracle name' = is oracle <$> findPlayerBy_ name name'

isPlayerOrphan :: MonadState Game m => Text -> m Bool
isPlayerOrphan name' = is orphan <$> findPlayerBy_ name name'

isPlayerProtector :: MonadState Game m => Text -> m Bool
isPlayerProtector name' = is protector <$> findPlayerBy_ name name'

isPlayerScapegoat :: MonadState Game m => Text -> m Bool
isPlayerScapegoat name' = is scapegoat <$> findPlayerBy_ name name'

isPlayerSeer :: MonadState Game m => Text -> m Bool
isPlayerSeer name' = is seer <$> findPlayerBy_ name name'

isPlayerSpitefulVillager :: MonadState Game m => Text -> m Bool
isPlayerSpitefulVillager name' = is spitefulVillager <$> findPlayerBy_ name name'

isPlayerWitch :: MonadState Game m => Text -> m Bool
isPlayerWitch name' = is witch <$> findPlayerBy_ name name'

isPlayerWerewolf :: MonadState Game m => Text -> m Bool
isPlayerWerewolf name' = is werewolf <$> findPlayerBy_ name name'

isPlayerAlive :: MonadState Game m => Text -> m Bool
isPlayerAlive name' = is alive <$> findPlayerBy_ name name'

isPlayerDead :: MonadState Game m => Text -> m Bool
isPlayerDead name' = is dead <$> findPlayerBy_ name name'
