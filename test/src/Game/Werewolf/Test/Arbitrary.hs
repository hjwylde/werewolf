{-|
Module      : Game.Werewolf.Test.Arbitrary
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Werewolf.Test.Arbitrary (
    -- * Contextual arbitraries
    arbitraryCommand, arbitrarySeeCommand, arbitraryKillVoteCommand, arbitraryLynchVoteCommand,
    arbitraryNewGame, arbitraryPlayer, arbitrarySeer, arbitraryVillager, arbitraryWerewolf,
) where

import Control.Lens hiding (elements)

import Data.List.Extra
import Data.Text       (Text, pack)

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Role    hiding (Villagers, Werewolves, name, _name)

import Test.QuickCheck

instance Show Command where
    show _ = "command"

arbitraryCommand :: Game -> Gen Command
arbitraryCommand game = oneof [arbitrarySeeCommand game, arbitraryKillVoteCommand game, arbitraryLynchVoteCommand game]

arbitrarySeeCommand :: Game -> Gen Command
arbitrarySeeCommand game = do
    caller <- arbitrarySeer game
    target <- arbitraryPlayer game

    return $ seeCommand (caller ^. name) (target ^. name)

arbitraryKillVoteCommand :: Game -> Gen Command
arbitraryKillVoteCommand game = do
    caller <- arbitraryWerewolf game
    target <- arbitraryPlayer game

    return $ killVoteCommand (caller ^. name) (target ^. name)

arbitraryLynchVoteCommand :: Game -> Gen Command
arbitraryLynchVoteCommand game = do
    caller <- arbitraryPlayer game
    target <- arbitraryPlayer game

    return $ lynchVoteCommand (caller ^. name) (target ^. name)

instance Arbitrary Game where
    arbitrary = do
        game <- arbitraryNewGame
        turn <- arbitrary

        return $ game { _turn = turn }

arbitraryNewGame :: Gen Game
arbitraryNewGame = do
    n <- choose (7, 24)
    players <- nubOn _name <$> infiniteList

    let seer        = head $ filterSeers players
    let werewolves  = take (n `quot` 6 + 1) $ filterWerewolves players
    let villagers   = take (n - 1 - (length werewolves)) $ filterVillagers players

    return $ newGame (seer:werewolves ++ villagers)

instance Arbitrary Turn where
    arbitrary = elements [Seers, Villagers, Werewolves, NoOne]

instance Arbitrary Player where
    arbitrary = newPlayer <$> arbitrary <*> arbitrary

arbitraryPlayer :: Game -> Gen Player
arbitraryPlayer = elements . filterAlive . _players

arbitrarySeer :: Game -> Gen Player
arbitrarySeer = elements . filterSeers . filterAlive . _players

arbitraryVillager :: Game -> Gen Player
arbitraryVillager = elements . filterWerewolves . filterAlive . _players

arbitraryWerewolf :: Game -> Gen Player
arbitraryWerewolf = elements . filterWerewolves . filterAlive . _players

instance Arbitrary State where
    arbitrary = elements [Alive, Dead]

instance Arbitrary Role where
    arbitrary = elements allRoles

instance Arbitrary Text where
    arbitrary = pack <$> vectorOf 6 (elements ['a'..'z'])
