{-|
Module      : Game.Werewolf.Test.Command.See
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Command.See (
    -- * Tests
    allSeeCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.Seer
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allSeeCommandTests :: [TestTree]
allSeeCommandTests =
    [ testProperty "see command errors when game is over"           prop_seeCommandErrorsWhenGameIsOver
    , testProperty "see command errors when caller does not exist"  prop_seeCommandErrorsWhenCallerDoesNotExist
    , testProperty "see command errors when target does not exist"  prop_seeCommandErrorsWhenTargetDoesNotExist
    , testProperty "see command errors when caller is dead"         prop_seeCommandErrorsWhenCallerIsDead
    , testProperty "see command errors when target is dead"         prop_seeCommandErrorsWhenTargetIsDead
    , testProperty "see command errors when not seer's turn"        prop_seeCommandErrorsWhenNotSeersTurn
    , testProperty "see command errors when caller not seer"        prop_seeCommandErrorsWhenCallerNotSeer
    , testProperty "see command sets see"                           prop_seeCommandSetsSee
    ]

prop_seeCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_seeCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitrarySeeCommand game) $ verbose_runCommandErrors game . getBlind

prop_seeCommandErrorsWhenCallerDoesNotExist :: GameAtSeersTurn -> Player -> Property
prop_seeCommandErrorsWhenCallerDoesNotExist (GameAtSeersTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = seeCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_seeCommandErrorsWhenTargetDoesNotExist :: GameAtSeersTurn -> Player -> Property
prop_seeCommandErrorsWhenTargetDoesNotExist (GameAtSeersTurn game) target = do
    let seer    = game ^?! players . seers
    let command = seeCommand (seer ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_seeCommandErrorsWhenCallerIsDead :: GameAtSeersTurn -> Property
prop_seeCommandErrorsWhenCallerIsDead (GameAtSeersTurn game) = do
    let seer    = game ^?! players . seers
    let game'   = killPlayer (seer ^. name) game

    forAll (arbitraryPlayer game') $ \target -> do
        let command = seeCommand (seer ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_seeCommandErrorsWhenTargetIsDead :: GameAtSeersTurn -> Property
prop_seeCommandErrorsWhenTargetIsDead (GameAtSeersTurn game) = do
    let seer = game ^?! players . seers

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = seeCommand (seer ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_seeCommandErrorsWhenNotSeersTurn :: Game -> Property
prop_seeCommandErrorsWhenNotSeersTurn game =
    hasn't (stage . _SeersTurn) game
    ==> forAll (arbitrarySeeCommand game) $ verbose_runCommandErrors game . getBlind

prop_seeCommandErrorsWhenCallerNotSeer :: GameAtSeersTurn -> Property
prop_seeCommandErrorsWhenCallerNotSeer (GameAtSeersTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't seer)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = seeCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_seeCommandSetsSee :: GameAtSeersTurn -> Property
prop_seeCommandSetsSee (GameAtSeersTurn game) =
    forAll (arbitrarySeeCommand game) $ \(Blind command) ->
    isJust $ run_ (apply command) game ^. see
