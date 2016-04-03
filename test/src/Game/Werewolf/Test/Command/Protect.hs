{-|
Module      : Game.Werewolf.Test.Command.Protect
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command.Protect (
    -- * Tests
    allProtectCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.Protector
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allProtectCommandTests :: [TestTree]
allProtectCommandTests =
    [ testProperty "protect command errors when game is over"               prop_protectCommandErrorsWhenGameIsOver
    , testProperty "protect command errors when caller does not exist"      prop_protectCommandErrorsWhenCallerDoesNotExist
    , testProperty "protect command errors when target does not exist"      prop_protectCommandErrorsWhenTargetDoesNotExist
    , testProperty "protect command errors when caller is dead"             prop_protectCommandErrorsWhenCallerIsDead
    , testProperty "protect command errors when target is dead"             prop_protectCommandErrorsWhenTargetIsDead
    , testProperty "protect command errors when not protector's turn"       prop_protectCommandErrorsWhenNotProtectorsTurn
    , testProperty "protect command errors when caller not protector"       prop_protectCommandErrorsWhenCallerNotProtector
    , testProperty "protect command errors when target is prior protect"    prop_protectCommandErrorsWhenTargetIsPriorProtect
    , testProperty "protect command sets prior protect"                     prop_protectCommandSetsPriorProtect
    , testProperty "protect command sets protect"                           prop_protectCommandSetsProtect
    ]

prop_protectCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_protectCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryProtectCommand game) $ verbose_runCommandErrors game . getBlind

prop_protectCommandErrorsWhenCallerDoesNotExist :: GameAtProtectorsTurn -> Player -> Property
prop_protectCommandErrorsWhenCallerDoesNotExist (GameAtProtectorsTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = protectCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetDoesNotExist :: GameAtProtectorsTurn -> Player -> Property
prop_protectCommandErrorsWhenTargetDoesNotExist (GameAtProtectorsTurn game) target = do
    let protector   = game ^?! players . protectors
    let command     = protectCommand (protector ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_protectCommandErrorsWhenCallerIsDead :: GameAtProtectorsTurn -> Property
prop_protectCommandErrorsWhenCallerIsDead (GameAtProtectorsTurn game) = do
    let protector   = game ^?! players . protectors
    let game'       = killPlayer (protector ^. name) game

    forAll (arbitraryPlayer game') $ \target -> do
        let command = protectCommand (protector ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_protectCommandErrorsWhenTargetIsDead :: GameAtProtectorsTurn -> Property
prop_protectCommandErrorsWhenTargetIsDead (GameAtProtectorsTurn game) = do
    let protector = game ^?! players . protectors

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = protectCommand (protector ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_protectCommandErrorsWhenNotProtectorsTurn :: Game -> Property
prop_protectCommandErrorsWhenNotProtectorsTurn game =
    hasn't (stage . _ProtectorsTurn) game
    ==> forAll (arbitraryProtectCommand game) $ verbose_runCommandErrors game . getBlind

prop_protectCommandErrorsWhenCallerNotProtector :: GameAtProtectorsTurn -> Property
prop_protectCommandErrorsWhenCallerNotProtector (GameAtProtectorsTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't protector)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = protectCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetIsPriorProtect :: GameWithProtect -> Property
prop_protectCommandErrorsWhenTargetIsPriorProtect (GameWithProtect game) = do
    let game' = game & protect .~ Nothing

    let protector   = game ^?! players . protectors
    let command     = protectCommand (protector ^. name) (fromJust $ game' ^. priorProtect)

    verbose_runCommandErrors game' command

prop_protectCommandSetsPriorProtect :: GameAtProtectorsTurn -> Property
prop_protectCommandSetsPriorProtect (GameAtProtectorsTurn game) =
    forAll (arbitraryProtectCommand game) $ \(Blind command) ->
    isJust $ run_ (apply command) game ^. priorProtect

prop_protectCommandSetsProtect :: GameAtProtectorsTurn -> Property
prop_protectCommandSetsProtect (GameAtProtectorsTurn game) =
    forAll (arbitraryProtectCommand game) $ \(Blind command) ->
    isJust $ run_ (apply command) game ^. protect
