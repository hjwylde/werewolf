{-|
Module      : Game.Werewolf.Test.Player
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Game.Werewolf.Test.Player (
    -- * Tests
    allPlayerTests,
) where

import Control.Lens

import Data.Text

import Game.Werewolf.Internal.Player
import Game.Werewolf.Role
import Game.Werewolf.Test.Arbitrary  ()

import Test.Tasty
import Test.Tasty.QuickCheck

allPlayerTests :: [TestTree]
allPlayerTests =
    [ testProperty "new player is alive" prop_newPlayerIsAlive
    ]

prop_newPlayerIsAlive :: Text -> Role -> Bool
prop_newPlayerIsAlive name role = newPlayer name role ^. state == Alive
