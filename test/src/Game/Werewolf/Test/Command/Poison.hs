{-|
Module      : Game.Werewolf.Test.Command.Poison
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command.Poison (
    -- * Tests
    allPoisonCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.Witch
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allPoisonCommandTests :: [TestTree]
allPoisonCommandTests =
    [ testProperty "poison command errors when game is over"            prop_poisonCommandErrorsWhenGameIsOver
    , testProperty "poison command errors when caller does not exist"   prop_poisonCommandErrorsWhenCallerDoesNotExist
    , testProperty "poison command errors when target does not exist"   prop_poisonCommandErrorsWhenTargetDoesNotExist
    , testProperty "poison command errors when caller is dead"          prop_poisonCommandErrorsWhenCallerIsDead
    , testProperty "poison command errors when target is dead"          prop_poisonCommandErrorsWhenTargetIsDead
    , testProperty "poison command errors when target is devoured"      prop_poisonCommandErrorsWhenTargetIsDevoured
    , testProperty "poison command errors when not witch's turn"        prop_poisonCommandErrorsWhenNotWitchsTurn
    , testProperty "poison command errors when caller has poisoned"     prop_poisonCommandErrorsWhenCallerHasPoisoned
    , testProperty "poison command errors when caller not witch"        prop_poisonCommandErrorsWhenCallerNotWitch
    , testProperty "poison command sets poison"                         prop_poisonCommandSetsPoison
    , testProperty "poison command sets poison used"                    prop_poisonCommandSetsPoisonUsed
    ]

prop_poisonCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_poisonCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryPoisonCommand game) $ verbose_runCommandErrors game . getBlind

prop_poisonCommandErrorsWhenCallerDoesNotExist :: GameAtWitchsTurn -> Player -> Property
prop_poisonCommandErrorsWhenCallerDoesNotExist (GameAtWitchsTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = poisonCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenTargetDoesNotExist :: GameAtWitchsTurn -> Player -> Property
prop_poisonCommandErrorsWhenTargetDoesNotExist (GameAtWitchsTurn game) target = do
    let witch   = game ^?! players . witches
    let command = poisonCommand (witch ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenCallerIsDead :: GameAtWitchsTurn -> Property
prop_poisonCommandErrorsWhenCallerIsDead (GameAtWitchsTurn game) = do
    let witch = game ^?! players . witches

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (witch ^. name) game
        let command = poisonCommand (witch ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_poisonCommandErrorsWhenTargetIsDead :: GameAtWitchsTurn -> Property
prop_poisonCommandErrorsWhenTargetIsDead (GameAtWitchsTurn game) = do
    let witch = game ^?! players . witches

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = poisonCommand (witch ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_poisonCommandErrorsWhenTargetIsDevoured :: GameWithDevourEvent -> Property
prop_poisonCommandErrorsWhenTargetIsDevoured (GameWithDevourEvent game) = do
    let witchsName  = game ^?! players . witches . name
    let targetName  = game ^?! events . traverse . _DevourEvent
    let command     = poisonCommand witchsName targetName

    verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_poisonCommandErrorsWhenNotWitchsTurn game =
    hasn't (stage . _WitchsTurn) game
    ==> forAll (arbitraryPoisonCommand game) $ verbose_runCommandErrors game . getBlind

prop_poisonCommandErrorsWhenCallerHasPoisoned :: GameWithPoison -> Property
prop_poisonCommandErrorsWhenCallerHasPoisoned (GameWithPoison game) = do
    let witch = game ^?! players . witches

    forAll (arbitraryPlayer game) $ \target -> do
        let command = poisonCommand (witch ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenCallerNotWitch :: GameAtWitchsTurn -> Property
prop_poisonCommandErrorsWhenCallerNotWitch (GameAtWitchsTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't witch)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = poisonCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_poisonCommandSetsPoison :: GameAtWitchsTurn -> Property
prop_poisonCommandSetsPoison (GameAtWitchsTurn game) =
    forAll (arbitraryPoisonCommand game) $ \(Blind command) ->
    isJust (run_ (apply command) game ^. poison)

prop_poisonCommandSetsPoisonUsed :: GameAtWitchsTurn -> Property
prop_poisonCommandSetsPoisonUsed (GameAtWitchsTurn game) =
    forAll (arbitraryPoisonCommand game) $ \(Blind command) ->
    run_ (apply command) game ^. poisonUsed
