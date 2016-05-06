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
    getAllowedVoters, getPendingVoters, getVoteResult,

    -- ** Queries
    isGameOver, isHuntersTurn, isOrphansTurn, isProtectorsTurn, isScapegoatsTurn, isSeersTurn,
    isSunrise, isVillagesTurn, isWerewolvesTurn, isWitchsTurn,
    hasAnyoneWon, hasFallenAngelWon, hasVillagersWon, hasWerewolvesWon,

    -- * Player

    -- ** Queries
    doesPlayerExist,
    isPlayerHunter, isPlayerJester, isPlayerOrphan, isPlayerProtector, isPlayerScapegoat,
    isPlayerSeer, isPlayerWitch,
    isPlayerWerewolf,
    isPlayerAlive, isPlayerDead,
) where

import Control.Lens         hiding (isn't)
import Control.Lens.Extra
import Control.Monad.Extra
import Control.Monad.Random
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import           Game.Werewolf.Game     hiding (getAllowedVoters, getPendingVoters, getVoteResult,
                                         hasAnyoneWon, hasFallenAngelWon, hasVillagersWon,
                                         hasWerewolvesWon)
import qualified Game.Werewolf.Game     as Game
import           Game.Werewolf.Messages
import           Game.Werewolf.Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role     hiding (name)

import Prelude hiding (round)

killPlayer :: (MonadState Game m, MonadWriter [Message] m) => Text -> m ()
killPlayer name' = do
    tell [playerKilledMessage name']

    players . traverse . filteredBy name name' . state .= Dead

removePlayer :: (MonadState Game m, MonadWriter [Message] m) => Text -> m ()
removePlayer name' = do
    killPlayer name'

    votes %= Map.delete name'

    player <- findPlayerBy_ name name'

    when (is fallenAngel player)    $ setPlayerAllegiance name' Villagers
    when (is orphan player)         $ roleModel .= Nothing
    when (is protector player)      $ do
        protect         .= Nothing
        priorProtect    .= Nothing
    when (is seer player)           $ see .= Nothing
    when (is witch player)          $ do
        heal        .= False
        healUsed    .= False
        poison      .= Nothing
        poisonUsed  .= False

-- | Fudges the player's allegiance. This function is useful for roles such as the Orphan where
--   they align themselves differently given some trigger.
setPlayerAllegiance :: MonadState Game m => Text -> Allegiance -> m ()
setPlayerAllegiance name' allegiance' = modify $ players . traverse . filteredBy name name' . role . allegiance .~ allegiance'

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

getVoteResult :: MonadState Game m => m [Player]
getVoteResult = gets Game.getVoteResult

isGameOver :: MonadState Game m => m Bool
isGameOver = has (stage . _GameOver) <$> get

isHuntersTurn :: MonadState Game m => m Bool
isHuntersTurn = orM
    [ has (stage . _HuntersTurn1) <$> get
    , has (stage . _HuntersTurn2) <$> get
    ]

isOrphansTurn :: MonadState Game m => m Bool
isOrphansTurn = has (stage . _OrphansTurn) <$> get

isProtectorsTurn :: MonadState Game m => m Bool
isProtectorsTurn = has (stage . _ProtectorsTurn) <$> get

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

isWitchsTurn :: MonadState Game m => m Bool
isWitchsTurn = has (stage . _WitchsTurn) <$> get

hasAnyoneWon :: MonadState Game m => m Bool
hasAnyoneWon = gets Game.hasAnyoneWon

hasFallenAngelWon :: MonadState Game m => m Bool
hasFallenAngelWon = gets Game.hasFallenAngelWon

hasVillagersWon :: MonadState Game m => m Bool
hasVillagersWon = gets Game.hasVillagersWon

hasWerewolvesWon :: MonadState Game m => m Bool
hasWerewolvesWon = gets Game.hasWerewolvesWon

doesPlayerExist :: MonadState Game m => Text -> m Bool
doesPlayerExist name = has (players . named name) <$> get

isPlayerHunter :: MonadState Game m => Text -> m Bool
isPlayerHunter name' = is hunter <$> findPlayerBy_ name name'

isPlayerJester :: MonadState Game m => Text -> m Bool
isPlayerJester name' = is jester <$> findPlayerBy_ name name'

isPlayerOrphan :: MonadState Game m => Text -> m Bool
isPlayerOrphan name' = is orphan <$> findPlayerBy_ name name'

isPlayerProtector :: MonadState Game m => Text -> m Bool
isPlayerProtector name' = is protector <$> findPlayerBy_ name name'

isPlayerScapegoat :: MonadState Game m => Text -> m Bool
isPlayerScapegoat name' = is scapegoat <$> findPlayerBy_ name name'

isPlayerSeer :: MonadState Game m => Text -> m Bool
isPlayerSeer name' = is seer <$> findPlayerBy_ name name'

isPlayerWitch :: MonadState Game m => Text -> m Bool
isPlayerWitch name' = is witch <$> findPlayerBy_ name name'

isPlayerWerewolf :: MonadState Game m => Text -> m Bool
isPlayerWerewolf name' = is werewolf <$> findPlayerBy_ name name'

isPlayerAlive :: MonadState Game m => Text -> m Bool
isPlayerAlive name' = is alive <$> findPlayerBy_ name name'

isPlayerDead :: MonadState Game m => Text -> m Bool
isPlayerDead name' = is dead <$> findPlayerBy_ name name'
