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

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
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
    , testProperty "protect command errors when not defender's turn"        prop_protectCommandErrorsWhenNotDefendersTurn
    , testProperty "protect command errors when caller not defender"        prop_protectCommandErrorsWhenCallerNotDefender
    , testProperty "protect command errors when target is prior protect"    prop_protectCommandErrorsWhenTargetIsPriorProtect
    , testProperty "protect command sets prior protect"                     prop_protectCommandSetsPriorProtect
    , testProperty "protect command sets protect"                           prop_protectCommandSetsProtect
    ]

prop_protectCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_protectCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryProtectCommand game) $ verbose_runCommandErrors game . getBlind

prop_protectCommandErrorsWhenCallerDoesNotExist :: GameAtDefendersTurn -> Player -> Property
prop_protectCommandErrorsWhenCallerDoesNotExist (GameAtDefendersTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = protectCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetDoesNotExist :: GameAtDefendersTurn -> Player -> Property
prop_protectCommandErrorsWhenTargetDoesNotExist (GameAtDefendersTurn game) target = do
    let defender    = game ^?! players . defenders
    let command     = protectCommand (defender ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_protectCommandErrorsWhenCallerIsDead :: GameAtDefendersTurn -> Property
prop_protectCommandErrorsWhenCallerIsDead (GameAtDefendersTurn game) = do
    let defender    = game ^?! players . defenders
    let game'       = killPlayer (defender ^. name) game

    forAll (arbitraryPlayer game') $ \target -> do
        let command = protectCommand (defender ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_protectCommandErrorsWhenTargetIsDead :: GameAtDefendersTurn -> Property
prop_protectCommandErrorsWhenTargetIsDead (GameAtDefendersTurn game) = do
    let defender = game ^?! players . defenders

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = protectCommand (defender ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_protectCommandErrorsWhenNotDefendersTurn :: Game -> Property
prop_protectCommandErrorsWhenNotDefendersTurn game =
    hasn't (stage . _DefendersTurn) game
    ==> forAll (arbitraryProtectCommand game) $ verbose_runCommandErrors game . getBlind

prop_protectCommandErrorsWhenCallerNotDefender :: GameAtDefendersTurn -> Property
prop_protectCommandErrorsWhenCallerNotDefender (GameAtDefendersTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't defender)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = protectCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetIsPriorProtect :: GameWithProtect -> Property
prop_protectCommandErrorsWhenTargetIsPriorProtect (GameWithProtect game) = do
    let game' = game & protect .~ Nothing

    let defender    = game ^?! players . defenders
    let command     = protectCommand (defender ^. name) (fromJust $ game' ^. priorProtect)

    verbose_runCommandErrors game' command

prop_protectCommandSetsPriorProtect :: GameAtDefendersTurn -> Property
prop_protectCommandSetsPriorProtect (GameAtDefendersTurn game) =
    forAll (arbitraryProtectCommand game) $ \(Blind command) ->
    isJust $ run_ (apply command) game ^. priorProtect

prop_protectCommandSetsProtect :: GameAtDefendersTurn -> Property
prop_protectCommandSetsProtect (GameAtDefendersTurn game) =
    forAll (arbitraryProtectCommand game) $ \(Blind command) ->
    isJust $ run_ (apply command) game ^. protect
