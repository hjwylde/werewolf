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
import Game.Werewolf.Command.Scapegoat as Scapegoat
import Game.Werewolf.Command.WildChild as WildChild
import Game.Werewolf.Command.WolfHound as WolfHound
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allChooseCommandTests :: [TestTree]
allChooseCommandTests =
    [ testProperty "wolf-hound choose command errors when game is over"                 prop_wolfHoundChooseCommandErrorsWhenGameIsOver
    , testProperty "wolf-hound choose command errors when caller does not exist"        prop_wolfHoundChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "wolf-hound choose command errors when caller is dead"               prop_wolfHoundChooseCommandErrorsWhenCallerIsDead
    , testProperty "wolf-hound choose command errors when not wolf-hound's turn"        prop_wolfHoundChooseCommandErrorsWhenNotWolfHoundsTurn
    , testProperty "wolf-hound choose command errors when caller not wolf-hound"        prop_wolfHoundChooseCommandErrorsWhenCallerNotWolfHound
    , testProperty "wolf-hound choose command errors when allegiance does not exist"    prop_wolfHoundChooseCommandErrorsWhenAllegianceDoesNotExist
    , testProperty "wolf-hound choose command sets allegiance chosen"                   prop_wolfHoundChooseCommandSetsAllegianceChosen

    , testProperty "wild-child choose command errors when game is over"             prop_wildChildChooseCommandErrorsWhenGameIsOver
    , testProperty "wild-child choose command errors when caller does not exist"    prop_wildChildChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "wild-child choose command errors when target does not exist"    prop_wildChildChooseCommandErrorsWhenTargetDoesNotExist
    , testProperty "wild-child choose command errors when caller is dead"           prop_wildChildChooseCommandErrorsWhenCallerIsDead
    , testProperty "wild-child choose command errors when target is dead"           prop_wildChildChooseCommandErrorsWhenTargetIsDead
    , testProperty "wild-child choose command errors when target is caller"         prop_wildChildChooseCommandErrorsWhenTargetIsCaller
    , testProperty "wild-child choose command errors when not wild-child's turn"    prop_wildChildChooseCommandErrorsWhenNotWildChildsTurn
    , testProperty "wild-child choose command errors when caller not wild-child"    prop_wildChildChooseCommandErrorsWhenCallerNotWildChild
    , testProperty "wild-child choose command sets role model"                      prop_wildChildChooseCommandSetsRoleModel

    , testProperty "scapegoat choose command errors when game is over"              prop_scapegoatChooseCommandErrorsWhenGameIsOver
    , testProperty "scapegoat choose command errors when caller does not exist"     prop_scapegoatChooseCommandErrorsWhenCallerDoesNotExist
    , testProperty "scapegoat choose command errors when any target does not exist" prop_scapegoatChooseCommandErrorsWhenAnyTargetDoesNotExist
    , testProperty "scapegoat choose command errors when any target is dead"        prop_scapegoatChooseCommandErrorsWhenAnyTargetIsDead
    , testProperty "scapegoat choose command errors when not scapegoat's turn"      prop_scapegoatChooseCommandErrorsWhenNotScapegoatsTurn
    , testProperty "scapegoat choose command errors when caller not scapegoat"      prop_scapegoatChooseCommandErrorsWhenCallerNotScapegoat
    , testProperty "scapegoat choose command sets allowed voters"                   prop_scapegoatChooseCommandSetsAllowedVoters
    , testProperty "scapegoat choose command resets scapegoat blamed"               prop_scapegoatChooseCommandResetsScapegoatBlamed
    ]

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

prop_wildChildChooseCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_wildChildChooseCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryWildChildChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_wildChildChooseCommandErrorsWhenCallerDoesNotExist :: GameAtWildChildsTurn -> Player -> Property
prop_wildChildChooseCommandErrorsWhenCallerDoesNotExist (GameAtWildChildsTurn game) caller =
    forAll (arbitraryPlayer game) $ \target -> do
        let command = WildChild.chooseCommand (caller ^. name) (target ^. name)

        not (doesPlayerExist (caller ^. name) game)
            ==> verbose_runCommandErrors game command

prop_wildChildChooseCommandErrorsWhenTargetDoesNotExist :: GameAtWildChildsTurn -> Player -> Property
prop_wildChildChooseCommandErrorsWhenTargetDoesNotExist (GameAtWildChildsTurn game) target = do
    let wildChild   = game ^?! players . wildChildren
    let command     = WildChild.chooseCommand (wildChild ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_wildChildChooseCommandErrorsWhenCallerIsDead :: GameAtWildChildsTurn -> Property
prop_wildChildChooseCommandErrorsWhenCallerIsDead (GameAtWildChildsTurn game) = do
    let wildChild   = game ^?! players . wildChildren
    let game'       = killPlayer (wildChild ^. name) game

    forAll (arbitraryPlayer game') $ \target -> do
        let command = WildChild.chooseCommand (wildChild ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_wildChildChooseCommandErrorsWhenTargetIsDead :: GameAtWildChildsTurn -> Property
prop_wildChildChooseCommandErrorsWhenTargetIsDead (GameAtWildChildsTurn game) = do
    let wildChild = game ^?! players . wildChildren

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = WildChild.chooseCommand (wildChild ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_wildChildChooseCommandErrorsWhenTargetIsCaller :: GameAtWildChildsTurn -> Property
prop_wildChildChooseCommandErrorsWhenTargetIsCaller (GameAtWildChildsTurn game) = do
    let wildChild   = game ^?! players . wildChildren
    let command     = WildChild.chooseCommand (wildChild ^. name) (wildChild ^. name)

    verbose_runCommandErrors game command

prop_wildChildChooseCommandErrorsWhenNotWildChildsTurn :: Game -> Property
prop_wildChildChooseCommandErrorsWhenNotWildChildsTurn game =
    hasn't (stage . _WildChildsTurn) game
    ==> forAll (arbitraryWildChildChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_wildChildChooseCommandErrorsWhenCallerNotWildChild :: GameAtWildChildsTurn -> Property
prop_wildChildChooseCommandErrorsWhenCallerNotWildChild (GameAtWildChildsTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't wildChild)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = WildChild.chooseCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_wildChildChooseCommandSetsRoleModel :: GameAtWildChildsTurn -> Property
prop_wildChildChooseCommandSetsRoleModel (GameAtWildChildsTurn game) = do
    let wildChild = game ^?! players . wildChildren

    forAll (suchThat (arbitraryPlayer game) (wildChild /=)) $ \target -> do
        let command = WildChild.chooseCommand (wildChild ^. name) (target ^. name)
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
