{-|
Module      : Game.Werewolf.Test.Arbitrary
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Werewolf.Test.Arbitrary (
    arbitraryCommand, arbitraryNewGame, arbitraryNewPlayer,
) where

import Control.Lens hiding (elements)

import Data.List.Extra
import Data.Text       (Text, pack)

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Role    hiding (_name)

import Test.QuickCheck

instance Arbitrary Command where
    arbitrary = Vote <$> arbitrary <*> arbitrary

arbitraryCommand :: Game -> Gen Command
arbitraryCommand game = Vote <$> elements (game ^. players) <*> elements (game ^. players)

instance Arbitrary Game where
    arbitrary = do
        n <- choose (7, 24)
        turn <- arbitrary
        players <- infiniteList

        return $ Game turn (take n $ nubOn _name players)

arbitraryNewGame :: Gen Game
arbitraryNewGame = do
    n <- choose (7, 24)
    players <- infiniteListOf arbitraryNewPlayer

    return $ newGame (take n $ nubOn _name players)

instance Arbitrary Turn where
    arbitrary = elements [newVillagersTurn, newWerewolvesTurn, NoOne]

instance Arbitrary Player where
    arbitrary = Player <$> arbitrary <*> arbitrary <*> arbitrary

arbitraryNewPlayer :: Gen Player
arbitraryNewPlayer = newPlayer <$> arbitrary <*> arbitrary

instance Arbitrary State where
    arbitrary = elements [Alive, Dead]

instance Arbitrary Role where
    arbitrary = elements allRoles

instance Arbitrary Text where
    arbitrary = pack <$> vectorOf 6 (elements ['a'..'z'])
