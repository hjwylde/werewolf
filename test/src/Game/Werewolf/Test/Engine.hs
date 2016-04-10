{-|
Module      : Game.Werewolf.Test.Engine
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Engine (
    -- * Tests
    allEngineTests,
) where

import Control.Lens         hiding (elements, isn't)
import Control.Monad.Except
import Control.Monad.Writer

import Data.Either.Extra

import Game.Werewolf
import Game.Werewolf.Test.Arbitrary ()

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allEngineTests :: [TestTree]
allEngineTests =
    [ testProperty "start game errors unless unique player names"               prop_startGameErrorsUnlessUniquePlayerNames
    , testProperty "start game errors when less than 7 players"                 prop_startGameErrorsWhenLessThan7Players
    , testProperty "start game errors when more than 1 of a restricted role"    prop_startGameErrorsWhenMoreThan1OfARestrictedRole
    ]

prop_startGameErrorsUnlessUniquePlayerNames :: Game -> Property
prop_startGameErrorsUnlessUniquePlayerNames game =
    forAll (elements players') $ \player ->
    isLeft (runExcept . runWriterT $ startGame "" (player:players'))
    where
        players' = game ^. players

prop_startGameErrorsWhenLessThan7Players :: [Player] -> Property
prop_startGameErrorsWhenLessThan7Players players =
    length players < 7
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan1OfARestrictedRole :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1OfARestrictedRole players =
    any (\role' -> length (players ^.. traverse . filteredBy role role') > 1) restrictedRoles
    ==> isLeft . runExcept . runWriterT $ startGame "" players
