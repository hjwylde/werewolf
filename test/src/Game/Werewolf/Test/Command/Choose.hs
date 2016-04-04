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

import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

import Game.Werewolf
import Game.Werewolf.Command.Orphan    as Orphan
import Game.Werewolf.Command.Scapegoat as Scapegoat
import Game.Werewolf.Command.WolfHound as WolfHound
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allChooseCommandTests :: [TestTree]
allChooseCommandTests =
    [ testProperty "orphan choose command errors when game is over"             prop_orphanChooseCommandErrorsWhenGameIsOver
    , testProperty "orphan choose command errors when caller does not exist"    prop_orphanChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "orphan choose command errors when target does not exist"    prop_orphanChooseCommandErrorsWhenTargetDoesNotExist
    , testProperty "orphan choose command errors when caller is dead"           prop_orphanChooseCommandErrorsWhenCallerIsDead
    , testProperty "orphan choose command errors when target is dead"           prop_orphanChooseCommandErrorsWhenTargetIsDead
    , testProperty "orphan choose command errors when target is caller"         prop_orphanChooseCommandErrorsWhenTargetIsCaller
    , testProperty "orphan choose command errors when not orphan's turn"        prop_orphanChooseCommandErrorsWhenNotOrphansTurn
    , testProperty "orphan choose command errors when caller not orphan"        prop_orphanChooseCommandErrorsWhenCallerNotOrphan
    , testProperty "orphan choose command sets role model"                      prop_orphanChooseCommandSetsRoleModel

    , testProperty "wolf-hound choose command errors when game is over"                 prop_wolfHoundChooseCommandErrorsWhenGameIsOver
    , testProperty "wolf-hound choose command errors when caller does not exist"        prop_wolfHoundChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "wolf-hound choose command errors when caller is dead"               prop_wolfHoundChooseCommandErrorsWhenCallerIsDead
    , testProperty "wolf-hound choose command errors when not wolf-hound's turn"        prop_wolfHoundChooseCommandErrorsWhenNotWolfHoundsTurn
    , testProperty "wolf-hound choose command errors when caller not wolf-hound"        prop_wolfHoundChooseCommandErrorsWhenCallerNotWolfHound
    , testProperty "wolf-hound choose command errors when allegiance does not exist"    prop_wolfHoundChooseCommandErrorsWhenAllegianceDoesNotExist
    , testProperty "wolf-hound choose command sets allegiance chosen"                   prop_wolfHoundChooseCommandSetsAllegianceChosen

    , testProperty "scapegoat choose command errors when game is over"              prop_scapegoatChooseCommandErrorsWhenGameIsOver
    , testProperty "scapegoat choose command errors when caller does not exist"     prop_scapegoatChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "scapegoat choose command errors when any target does not exist" prop_scapegoatChooseCommandErrorsWhenAnyTargetDoesNotExist
    , testProperty "scapegoat choose command errors when any target is dead"        prop_scapegoatChooseCommandErrorsWhenAnyTargetIsDead
    , testProperty "scapegoat choose command errors when not scapegoat's turn"      prop_scapegoatChooseCommandErrorsWhenNotScapegoatsTurn
    , testProperty "scapegoat choose command errors when caller not scapegoat"      prop_scapegoatChooseCommandErrorsWhenCallerNotScapegoat
    , testProperty "scapegoat choose command sets allowed voters"                   prop_scapegoatChooseCommandSetsAllowedVoters
    , testProperty "scapegoat choose command resets scapegoat blamed"               prop_scapegoatChooseCommandResetsScapegoatBlamed
    ]

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

prop_wolfHoundChooseCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_wolfHoundChooseCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryWolfHoundChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_wolfHoundChooseCommandErrorsWhenCallerDoesNotExist :: GameAtWolfHoundsTurn -> Player -> Allegiance -> Property
prop_wolfHoundChooseCommandErrorsWhenCallerDoesNotExist (GameAtWolfHoundsTurn game) caller allegiance = do
    let command = WolfHound.chooseCommand (caller ^. name) (T.pack $ show allegiance)

    not (doesPlayerExist (caller ^. name) game)
        ==> verbose_runCommandErrors game command

prop_wolfHoundChooseCommandErrorsWhenCallerIsDead :: GameAtWolfHoundsTurn -> Allegiance -> Property
prop_wolfHoundChooseCommandErrorsWhenCallerIsDead (GameAtWolfHoundsTurn game) allegiance = do
    let wolfHound   = game ^?! players . wolfHounds
    let game'       = killPlayer (wolfHound ^. name) game
    let command     = WolfHound.chooseCommand (wolfHound ^. name) (T.pack $ show allegiance)

    verbose_runCommandErrors game' command

prop_wolfHoundChooseCommandErrorsWhenNotWolfHoundsTurn :: Game -> Property
prop_wolfHoundChooseCommandErrorsWhenNotWolfHoundsTurn game =
    hasn't (stage . _WolfHoundsTurn) game
    ==> forAll (arbitraryWolfHoundChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_wolfHoundChooseCommandErrorsWhenCallerNotWolfHound :: GameAtWolfHoundsTurn -> Allegiance -> Property
prop_wolfHoundChooseCommandErrorsWhenCallerNotWolfHound (GameAtWolfHoundsTurn game) allegiance =
    forAll (suchThat (arbitraryPlayer game) (isn't wolfHound)) $ \caller -> do
        let command = WolfHound.chooseCommand (caller ^. name) (T.pack $ show allegiance)

        verbose_runCommandErrors game command

prop_wolfHoundChooseCommandErrorsWhenAllegianceDoesNotExist :: GameAtWolfHoundsTurn -> Text -> Property
prop_wolfHoundChooseCommandErrorsWhenAllegianceDoesNotExist (GameAtWolfHoundsTurn game) allegiance = do
    let wolfHound   = game ^?! players . wolfHounds
    let command     = WolfHound.chooseCommand (wolfHound ^. name) allegiance

    allegiance `notElem` ["Villagers", "Werewolves"]
        ==> verbose_runCommandErrors game command

prop_wolfHoundChooseCommandSetsAllegianceChosen :: GameAtWolfHoundsTurn -> Property
prop_wolfHoundChooseCommandSetsAllegianceChosen (GameAtWolfHoundsTurn game) = do
    let wolfHoundsName = game ^?! players . wolfHounds . name

    forAll (elements [Villagers, Werewolves]) $ \allegiance' -> do
        let command = WolfHound.chooseCommand wolfHoundsName (T.pack $ show allegiance')
        let game'   = run_ (apply command) game

        fromJust (game' ^. allegianceChosen) === allegiance'

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
