{-|
Module      : Game.Werewolf.Test.Command.Choose
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command.Choose (
    -- * Tests
    allChooseCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.Hunter    as Hunter
import Game.Werewolf.Command.Orphan    as Orphan
import Game.Werewolf.Command.Scapegoat as Scapegoat
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allChooseCommandTests :: [TestTree]
allChooseCommandTests =
    [ testProperty "hunter choose command errors when game is over"                 prop_hunterChooseCommandErrorsWhenGameIsOver
    , testProperty "hunter choose command errors when caller does not exist"        prop_hunterChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "hunter choose command errors when any target does not exist"    prop_hunterChooseCommandErrorsWhenTargetDoesNotExist
    , testProperty "hunter choose command errors when any target is dead"           prop_hunterChooseCommandErrorsWhenTargetIsDead
    , testProperty "hunter choose command errors when not hunter's turn"            prop_hunterChooseCommandErrorsWhenNotHuntersTurn
    , testProperty "hunter choose command errors when caller not hunter"            prop_hunterChooseCommandErrorsWhenCallerNotHunter
    , testProperty "hunter choose command kills target"                             prop_hunterChooseCommandKillsTarget
    , testProperty "hunter choose command sets hunter retaliated"                   prop_hunterChooseCommandSetsHunterRetaliated

    , testProperty "orphan choose command errors when game is over"             prop_orphanChooseCommandErrorsWhenGameIsOver
    , testProperty "orphan choose command errors when caller does not exist"    prop_orphanChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "orphan choose command errors when target does not exist"    prop_orphanChooseCommandErrorsWhenTargetDoesNotExist
    , testProperty "orphan choose command errors when caller is dead"           prop_orphanChooseCommandErrorsWhenCallerIsDead
    , testProperty "orphan choose command errors when target is dead"           prop_orphanChooseCommandErrorsWhenTargetIsDead
    , testProperty "orphan choose command errors when target is caller"         prop_orphanChooseCommandErrorsWhenTargetIsCaller
    , testProperty "orphan choose command errors when not orphan's turn"        prop_orphanChooseCommandErrorsWhenNotOrphansTurn
    , testProperty "orphan choose command errors when caller not orphan"        prop_orphanChooseCommandErrorsWhenCallerNotOrphan
    , testProperty "orphan choose command sets role model"                      prop_orphanChooseCommandSetsRoleModel

    , testProperty "scapegoat choose command errors when game is over"              prop_scapegoatChooseCommandErrorsWhenGameIsOver
    , testProperty "scapegoat choose command errors when caller does not exist"     prop_scapegoatChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "scapegoat choose command errors when any target does not exist" prop_scapegoatChooseCommandErrorsWhenAnyTargetDoesNotExist
    , testProperty "scapegoat choose command errors when any target is dead"        prop_scapegoatChooseCommandErrorsWhenAnyTargetIsDead
    , testProperty "scapegoat choose command errors when not scapegoat's turn"      prop_scapegoatChooseCommandErrorsWhenNotScapegoatsTurn
    , testProperty "scapegoat choose command errors when caller not scapegoat"      prop_scapegoatChooseCommandErrorsWhenCallerNotScapegoat
    , testProperty "scapegoat choose command sets allowed voters"                   prop_scapegoatChooseCommandSetsAllowedVoters
    , testProperty "scapegoat choose command resets scapegoat blamed"               prop_scapegoatChooseCommandResetsScapegoatBlamed
    ]

prop_hunterChooseCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_hunterChooseCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryHunterChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_hunterChooseCommandErrorsWhenCallerDoesNotExist :: GameAtHuntersTurn -> Player -> Property
prop_hunterChooseCommandErrorsWhenCallerDoesNotExist (GameAtHuntersTurn game) caller =
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Hunter.chooseCommand (caller ^. name) (target ^. name)

        not (doesPlayerExist (caller ^. name) game)
            ==> verbose_runCommandErrors game command

prop_hunterChooseCommandErrorsWhenTargetDoesNotExist :: GameAtHuntersTurn -> Player -> Property
prop_hunterChooseCommandErrorsWhenTargetDoesNotExist (GameAtHuntersTurn game) target = do
    let hunter  = game ^?! players . hunters
    let command = Hunter.chooseCommand (hunter ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_hunterChooseCommandErrorsWhenTargetIsDead :: GameAtHuntersTurn -> Property
prop_hunterChooseCommandErrorsWhenTargetIsDead (GameAtHuntersTurn game) = do
    let hunter = game ^?! players . hunters

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = Hunter.chooseCommand (hunter ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_hunterChooseCommandErrorsWhenNotHuntersTurn :: Game -> Property
prop_hunterChooseCommandErrorsWhenNotHuntersTurn game =
    hasn't (stage . _HuntersTurn1) game && hasn't (stage . _HuntersTurn2) game
    ==> forAll (arbitraryHunterChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_hunterChooseCommandErrorsWhenCallerNotHunter :: GameAtHuntersTurn -> Property
prop_hunterChooseCommandErrorsWhenCallerNotHunter (GameAtHuntersTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't hunter)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Hunter.chooseCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_hunterChooseCommandKillsTarget :: GameAtHuntersTurn -> Property
prop_hunterChooseCommandKillsTarget (GameAtHuntersTurn game) = do
    let hunter = game ^?! players . hunters

    forAll (arbitraryPlayer game) $ \target -> do
        let command = Hunter.chooseCommand (hunter ^. name) (target ^. name)
        let game'   = run_ (apply command) game

        is dead $ game' ^?! players . traverse . filteredBy name (target ^. name)

prop_hunterChooseCommandSetsHunterRetaliated :: GameAtHuntersTurn -> Property
prop_hunterChooseCommandSetsHunterRetaliated (GameAtHuntersTurn game) = do
    let hunter = game ^?! players . hunters

    forAll (arbitraryPlayer game) $ \target -> do
        let command = Hunter.chooseCommand (hunter ^. name) (target ^. name)
        let game'   = run_ (apply command) game

        game' ^. hunterRetaliated

prop_orphanChooseCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_orphanChooseCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryOrphanChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_orphanChooseCommandErrorsWhenCallerDoesNotExist :: GameAtOrphansTurn -> Player -> Property
prop_orphanChooseCommandErrorsWhenCallerDoesNotExist (GameAtOrphansTurn game) caller =
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Orphan.chooseCommand (caller ^. name) (target ^. name)

        not (doesPlayerExist (caller ^. name) game)
            ==> verbose_runCommandErrors game command

prop_orphanChooseCommandErrorsWhenTargetDoesNotExist :: GameAtOrphansTurn -> Player -> Property
prop_orphanChooseCommandErrorsWhenTargetDoesNotExist (GameAtOrphansTurn game) target = do
    let orphan  = game ^?! players . orphans
    let command = Orphan.chooseCommand (orphan ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_orphanChooseCommandErrorsWhenCallerIsDead :: GameAtOrphansTurn -> Property
prop_orphanChooseCommandErrorsWhenCallerIsDead (GameAtOrphansTurn game) = do
    let orphan  = game ^?! players . orphans
    let game'   = killPlayer (orphan ^. name) game

    forAll (arbitraryPlayer game') $ \target -> do
        let command = Orphan.chooseCommand (orphan ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_orphanChooseCommandErrorsWhenTargetIsDead :: GameAtOrphansTurn -> Property
prop_orphanChooseCommandErrorsWhenTargetIsDead (GameAtOrphansTurn game) = do
    let orphan = game ^?! players . orphans

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = Orphan.chooseCommand (orphan ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_orphanChooseCommandErrorsWhenTargetIsCaller :: GameAtOrphansTurn -> Property
prop_orphanChooseCommandErrorsWhenTargetIsCaller (GameAtOrphansTurn game) = do
    let orphan  = game ^?! players . orphans
    let command = Orphan.chooseCommand (orphan ^. name) (orphan ^. name)

    verbose_runCommandErrors game command

prop_orphanChooseCommandErrorsWhenNotOrphansTurn :: Game -> Property
prop_orphanChooseCommandErrorsWhenNotOrphansTurn game =
    hasn't (stage . _OrphansTurn) game
    ==> forAll (arbitraryOrphanChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_orphanChooseCommandErrorsWhenCallerNotOrphan :: GameAtOrphansTurn -> Property
prop_orphanChooseCommandErrorsWhenCallerNotOrphan (GameAtOrphansTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't orphan)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Orphan.chooseCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_orphanChooseCommandSetsRoleModel :: GameAtOrphansTurn -> Property
prop_orphanChooseCommandSetsRoleModel (GameAtOrphansTurn game) = do
    let orphan = game ^?! players . orphans

    forAll (suchThat (arbitraryPlayer game) (orphan /=)) $ \target -> do
        let command = Orphan.chooseCommand (orphan ^. name) (target ^. name)
        let game'   = run_ (apply command) game

        fromJust (game' ^. roleModel) === target ^. name

prop_scapegoatChooseCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_scapegoatChooseCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryScapegoatChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_scapegoatChooseCommandErrorsWhenCallerDoesNotExist :: GameAtScapegoatsTurn -> Player -> Property
prop_scapegoatChooseCommandErrorsWhenCallerDoesNotExist (GameAtScapegoatsTurn game) caller =
    forAll (NonEmpty <$> sublistOf (game ^.. players . traverse . alive)) $ \(NonEmpty targets) -> do
        let command = Scapegoat.chooseCommand (caller ^. name) (targets ^.. names)

        not (doesPlayerExist (caller ^. name) game)
            ==> verbose_runCommandErrors game command

prop_scapegoatChooseCommandErrorsWhenAnyTargetDoesNotExist :: GameAtScapegoatsTurn -> Player -> Property
prop_scapegoatChooseCommandErrorsWhenAnyTargetDoesNotExist (GameAtScapegoatsTurn game) target = do
    let scapegoat   = game ^?! players . scapegoats
    let command     = Scapegoat.chooseCommand (scapegoat ^. name) [target ^. name]

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_scapegoatChooseCommandErrorsWhenAnyTargetIsDead :: GameAtScapegoatsTurn -> Property
prop_scapegoatChooseCommandErrorsWhenAnyTargetIsDead (GameAtScapegoatsTurn game) = do
    let scapegoat = game ^?! players . scapegoats

    forAll (NonEmpty <$> sublistOf (game ^.. players . traverse . alive)) $ \(NonEmpty targets) ->
        forAll (elements targets) $ \target -> do
            let game'   = killPlayer (target ^. name) game
            let command = Scapegoat.chooseCommand (scapegoat ^. name) (targets ^.. names)

            verbose_runCommandErrors game' command

prop_scapegoatChooseCommandErrorsWhenNotScapegoatsTurn :: Game -> Property
prop_scapegoatChooseCommandErrorsWhenNotScapegoatsTurn game =
    hasn't (stage . _ScapegoatsTurn) game
    ==> forAll (arbitraryScapegoatChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_scapegoatChooseCommandErrorsWhenCallerNotScapegoat :: GameAtScapegoatsTurn -> Property
prop_scapegoatChooseCommandErrorsWhenCallerNotScapegoat (GameAtScapegoatsTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't scapegoat)) $ \caller ->
    forAll (NonEmpty <$> sublistOf (game ^.. players . traverse . alive)) $ \(NonEmpty targets) -> do
        let command = Scapegoat.chooseCommand (caller ^. name) (targets ^.. names)

        verbose_runCommandErrors game command

prop_scapegoatChooseCommandSetsAllowedVoters :: GameAtScapegoatsTurn -> Property
prop_scapegoatChooseCommandSetsAllowedVoters (GameAtScapegoatsTurn game) = do
    let scapegoat = game ^?! players . scapegoats

    forAll (NonEmpty <$> sublistOf (game ^.. players . traverse . alive)) $ \(NonEmpty targets) -> do
        let command = Scapegoat.chooseCommand (scapegoat ^. name) (targets ^.. names)
        let game'   = run_ (apply command) game

        game' ^. allowedVoters === targets ^.. names

prop_scapegoatChooseCommandResetsScapegoatBlamed :: GameAtScapegoatsTurn -> Property
prop_scapegoatChooseCommandResetsScapegoatBlamed (GameAtScapegoatsTurn game) = do
    forAll (arbitraryScapegoatChooseCommand game) $ \(Blind command) ->
        not $ run_ (apply command) game ^. scapegoatBlamed
