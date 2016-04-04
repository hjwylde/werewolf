{-|
Module      : Game.Werewolf.Test.Command.Quit
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command.Quit (
    -- * Tests
    allQuitCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.Global
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allQuitCommandTests :: [TestTree]
allQuitCommandTests =
    [ testProperty "quit command errors when game is over"                              prop_quitCommandErrorsWhenGameIsOver
    , testProperty "quit command errors when caller does not exist"                     prop_quitCommandErrorsWhenCallerDoesNotExist
    , testProperty "quit command errors when caller is dead"                            prop_quitCommandErrorsWhenCallerIsDead
    , testProperty "quit command kills player"                                          prop_quitCommandKillsPlayer
    , testProperty "quit command clears allegiance chosen when caller is wolf-hound"    prop_quitCommandClearsAllegianceChosenWhenCallerIsWolfHound
    , testProperty "quit command clears heal when caller is witch"                      prop_quitCommandClearsHealWhenCallerIsWitch
    , testProperty "quit command clears heal used when caller is witch"                 prop_quitCommandClearsHealUsedWhenCallerIsWitch
    , testProperty "quit command clears poison when caller is witch"                    prop_quitCommandClearsPoisonWhenCallerIsWitch
    , testProperty "quit command clears poison used when caller is witch"               prop_quitCommandClearsPoisonUsedWhenCallerIsWitch
    , testProperty "quit command clears prior protect when caller is protector"         prop_quitCommandClearsPriorProtectWhenCallerIsProtector
    , testProperty "quit command clears protect when caller is protector"               prop_quitCommandClearsProtectWhenCallerIsProtector
    , testProperty "quit command clears player's devour vote"                           prop_quitCommandClearsPlayersDevourVote
    , testProperty "quit command clears player's lynch vote"                            prop_quitCommandClearsPlayersLynchVote
    , testProperty "quit command clears role model when caller is orphan"               prop_quitCommandClearsRoleModelWhenCallerIsOrphan
    , testProperty "quit command sets angel's allegiance when caller is angel"          prop_quitCommandSetsAngelsAllegianceWhenCallerIsAngel
    ]

prop_quitCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_quitCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryQuitCommand game) $ verbose_runCommandErrors game . getBlind

prop_quitCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_quitCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (quitCommand $ caller ^. name)

prop_quitCommandErrorsWhenCallerIsDead :: Game -> Property
prop_quitCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = quitCommand $ caller ^. name

        verbose_runCommandErrors game' command

prop_quitCommandKillsPlayer :: Game -> Property
prop_quitCommandKillsPlayer game =
    hasn't (stage . _GameOver) game
    ==> forAll (arbitraryQuitCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        length (game' ^.. players . traverse . dead) == 1

prop_quitCommandClearsAllegianceChosenWhenCallerIsWolfHound :: GameWithAllegianceChosen -> Bool
prop_quitCommandClearsAllegianceChosenWhenCallerIsWolfHound (GameWithAllegianceChosen game) = do
    let wolfHoundsName  = game ^?! players . wolfHounds . name
    let command         = quitCommand wolfHoundsName

    run_ (apply command) game ^. allegianceChosen

prop_quitCommandClearsHealWhenCallerIsWitch :: GameWithHeal -> Bool
prop_quitCommandClearsHealWhenCallerIsWitch (GameWithHeal game) = do
    let witch   = game ^?! players . witches
    let command = quitCommand (witch ^. name)

    not $ run_ (apply command) game ^. heal

prop_quitCommandClearsHealUsedWhenCallerIsWitch :: GameWithHeal -> Bool
prop_quitCommandClearsHealUsedWhenCallerIsWitch (GameWithHeal game) = do
    let witch   = game ^?! players . witches
    let command = quitCommand (witch ^. name)

    not $ run_ (apply command) game ^. healUsed

prop_quitCommandClearsPoisonWhenCallerIsWitch :: GameWithPoison -> Bool
prop_quitCommandClearsPoisonWhenCallerIsWitch (GameWithPoison game) = do
    let witch   = game ^?! players . witches
    let command = quitCommand (witch ^. name)

    isNothing $ run_ (apply command) game ^. poison

prop_quitCommandClearsPoisonUsedWhenCallerIsWitch :: GameWithPoison -> Bool
prop_quitCommandClearsPoisonUsedWhenCallerIsWitch (GameWithPoison game) = do
    let witch   = game ^?! players . witches
    let command = quitCommand (witch ^. name)

    not $ run_ (apply command) game ^. poisonUsed

prop_quitCommandClearsPriorProtectWhenCallerIsProtector :: GameWithProtect -> Bool
prop_quitCommandClearsPriorProtectWhenCallerIsProtector (GameWithProtect game) = do
    let protector   = game ^?! players . protectors
    let command     = quitCommand (protector ^. name)

    isNothing $ run_ (apply command) game ^. priorProtect

prop_quitCommandClearsProtectWhenCallerIsProtector :: GameWithProtect -> Bool
prop_quitCommandClearsProtectWhenCallerIsProtector (GameWithProtect game) = do
    let protector   = game ^?! players . protectors
    let command     = quitCommand (protector ^. name)

    isNothing $ run_ (apply command) game ^. protect

prop_quitCommandClearsPlayersDevourVote :: GameWithDevourVotes -> Property
prop_quitCommandClearsPlayersDevourVote (GameWithDevourVotes game) =
    forAll (arbitraryWerewolf game) $ \caller -> do
        let command = quitCommand (caller ^. name)

        isNothing $ run_ (apply command) game ^. votes . at (caller ^. name)

prop_quitCommandClearsPlayersLynchVote :: GameWithLynchVotes -> Property
prop_quitCommandClearsPlayersLynchVote (GameWithLynchVotes game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let command = quitCommand (caller ^. name)

        isNothing $ run_ (apply command) game ^. votes . at (caller ^. name)

prop_quitCommandClearsRoleModelWhenCallerIsOrphan :: GameWithRoleModel -> Bool
prop_quitCommandClearsRoleModelWhenCallerIsOrphan (GameWithRoleModel game) = do
    let orphan  = game ^?! players . orphans
    let command = quitCommand (orphan ^. name)

    isNothing $ run_ (apply command) game ^. roleModel

prop_quitCommandSetsAngelsAllegianceWhenCallerIsAngel :: Game -> Bool
prop_quitCommandSetsAngelsAllegianceWhenCallerIsAngel game = do
    let angelsName  = game ^?! players . angels . name
    let command     = quitCommand angelsName
    let game'       = run_ (apply command) game

    is villager $ game' ^?! players . angels
