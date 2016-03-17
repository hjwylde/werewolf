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

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Role           hiding (name)
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allChooseCommandTests :: [TestTree]
allChooseCommandTests =
    [ testProperty "choose allegiance command errors when game is over"                 prop_chooseAllegianceCommandErrorsWhenGameIsOver
    , testProperty "choose allegiance command errors when caller does not exist"        prop_chooseAllegianceCommandErrorsWhenCallerDoesNotExist
    , testProperty "choose allegiance command errors when caller is dead"               prop_chooseAllegianceCommandErrorsWhenCallerIsDead
    , testProperty "choose allegiance command errors when not wolf-hound's turn"        prop_chooseAllegianceCommandErrorsWhenNotWolfHoundsTurn
    , testProperty "choose allegiance command errors when caller not wolf-hound"        prop_chooseAllegianceCommandErrorsWhenCallerNotWolfHound
    , testProperty "choose allegiance command errors when allegiance does not exist"    prop_chooseAllegianceCommandErrorsWhenAllegianceDoesNotExist
    , testProperty "choose allegiance command sets allegiance chosen"                   prop_chooseAllegianceCommandSetsAllegianceChosen

    , testProperty "choose player command errors when game is over"             prop_choosePlayerCommandErrorsWhenGameIsOver
    , testProperty "choose player command errors when caller does not exist"    prop_choosePlayerCommandErrorsWhenCallerDoesNotExist
    , testProperty "choose player command errors when target does not exist"    prop_choosePlayerCommandErrorsWhenTargetDoesNotExist
    , testProperty "choose player command errors when caller is dead"           prop_choosePlayerCommandErrorsWhenCallerIsDead
    , testProperty "choose player command errors when target is dead"           prop_choosePlayerCommandErrorsWhenTargetIsDead
    , testProperty "choose player command errors when target is caller"         prop_choosePlayerCommandErrorsWhenTargetIsCaller
    , testProperty "choose player command errors when not wild-child's turn"    prop_choosePlayerCommandErrorsWhenNotWildChildsTurn
    , testProperty "choose player command errors when caller not wild-child"    prop_choosePlayerCommandErrorsWhenCallerNotWildChild
    , testProperty "choose player command sets role model"                      prop_choosePlayerCommandSetsRoleModel

    , testProperty "choose players command errors when game is over"                prop_choosePlayersCommandErrorsWhenGameIsOver
    , testProperty "choose players command errors when caller does not exist"       prop_choosePlayersCommandErrorsWhenCallerDoesNotExist
    , testProperty "choose players command errors when any target does not exist"   prop_choosePlayersCommandErrorsWhenAnyTargetDoesNotExist
    , testProperty "choose players command errors when any target is dead"          prop_choosePlayersCommandErrorsWhenAnyTargetIsDead
    , testProperty "choose players command errors when not scapegoat's turn"        prop_choosePlayersCommandErrorsWhenNotScapegoatsTurn
    , testProperty "choose players command errors when caller not scapegoat"        prop_choosePlayersCommandErrorsWhenCallerNotScapegoat
    , testProperty "choose players command sets allowed voters"                     prop_choosePlayersCommandSetsAllowedVoters
    , testProperty "choose players command resets scapegoat blamed"                 prop_choosePlayersCommandResetsScapegoatBlamed
    ]

prop_chooseAllegianceCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_chooseAllegianceCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryChooseAllegianceCommand game) $ verbose_runCommandErrors game . getBlind

prop_chooseAllegianceCommandErrorsWhenCallerDoesNotExist :: GameAtWolfHoundsTurn -> Player -> Allegiance -> Property
prop_chooseAllegianceCommandErrorsWhenCallerDoesNotExist (GameAtWolfHoundsTurn game) caller allegiance = do
    let command = chooseAllegianceCommand (caller ^. name) (T.pack $ show allegiance)

    not (doesPlayerExist (caller ^. name) game)
        ==> verbose_runCommandErrors game command

prop_chooseAllegianceCommandErrorsWhenCallerIsDead :: GameAtWolfHoundsTurn -> Allegiance -> Property
prop_chooseAllegianceCommandErrorsWhenCallerIsDead (GameAtWolfHoundsTurn game) allegiance = do
    let wolfHound   = game ^?! players . wolfHounds
    let game'       = killPlayer (wolfHound ^. name) game
    let command     = chooseAllegianceCommand (wolfHound ^. name) (T.pack $ show allegiance)

    verbose_runCommandErrors game' command

prop_chooseAllegianceCommandErrorsWhenNotWolfHoundsTurn :: Game -> Property
prop_chooseAllegianceCommandErrorsWhenNotWolfHoundsTurn game =
    hasn't (stage . _WolfHoundsTurn) game
    ==> forAll (arbitraryChooseAllegianceCommand game) $ verbose_runCommandErrors game . getBlind

prop_chooseAllegianceCommandErrorsWhenCallerNotWolfHound :: GameAtWolfHoundsTurn -> Allegiance -> Property
prop_chooseAllegianceCommandErrorsWhenCallerNotWolfHound (GameAtWolfHoundsTurn game) allegiance =
    forAll (suchThat (arbitraryPlayer game) (isn't wolfHound)) $ \caller -> do
        let command = chooseAllegianceCommand (caller ^. name) (T.pack $ show allegiance)

        verbose_runCommandErrors game command

prop_chooseAllegianceCommandErrorsWhenAllegianceDoesNotExist :: GameAtWolfHoundsTurn -> Text -> Property
prop_chooseAllegianceCommandErrorsWhenAllegianceDoesNotExist (GameAtWolfHoundsTurn game) allegiance = do
    let wolfHound   = game ^?! players . wolfHounds
    let command     = chooseAllegianceCommand (wolfHound ^. name) allegiance

    allegiance `notElem` ["Villagers", "Werewolves"]
        ==> verbose_runCommandErrors game command

prop_chooseAllegianceCommandSetsAllegianceChosen :: GameAtWolfHoundsTurn -> Property
prop_chooseAllegianceCommandSetsAllegianceChosen (GameAtWolfHoundsTurn game) = do
    let wolfHoundsName = game ^?! players . wolfHounds . name

    forAll (elements [Villagers, Werewolves]) $ \allegiance' -> do
        let command = chooseAllegianceCommand wolfHoundsName (T.pack $ show allegiance')
        let game'   = run_ (apply command) game

        fromJust (game' ^. allegianceChosen) === allegiance'

prop_choosePlayerCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_choosePlayerCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryChoosePlayerCommand game) $ verbose_runCommandErrors game . getBlind

prop_choosePlayerCommandErrorsWhenCallerDoesNotExist :: GameAtWildChildsTurn -> Player -> Property
prop_choosePlayerCommandErrorsWhenCallerDoesNotExist (GameAtWildChildsTurn game) caller =
    forAll (arbitraryPlayer game) $ \target -> do
        let command = choosePlayerCommand (caller ^. name) (target ^. name)

        not (doesPlayerExist (caller ^. name) game)
            ==> verbose_runCommandErrors game command

prop_choosePlayerCommandErrorsWhenTargetDoesNotExist :: GameAtWildChildsTurn -> Player -> Property
prop_choosePlayerCommandErrorsWhenTargetDoesNotExist (GameAtWildChildsTurn game) target = do
    let wildChild   = game ^?! players . wildChildren
    let command     = choosePlayerCommand (wildChild ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_choosePlayerCommandErrorsWhenCallerIsDead :: GameAtWildChildsTurn -> Property
prop_choosePlayerCommandErrorsWhenCallerIsDead (GameAtWildChildsTurn game) = do
    let wildChild   = game ^?! players . wildChildren
    let game'       = killPlayer (wildChild ^. name) game

    forAll (arbitraryPlayer game') $ \target -> do
        let command = choosePlayerCommand (wildChild ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_choosePlayerCommandErrorsWhenTargetIsDead :: GameAtWildChildsTurn -> Property
prop_choosePlayerCommandErrorsWhenTargetIsDead (GameAtWildChildsTurn game) = do
    let wildChild = game ^?! players . wildChildren

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = choosePlayerCommand (wildChild ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_choosePlayerCommandErrorsWhenTargetIsCaller :: GameAtWildChildsTurn -> Property
prop_choosePlayerCommandErrorsWhenTargetIsCaller (GameAtWildChildsTurn game) = do
    let wildChild   = game ^?! players . wildChildren
    let command     = choosePlayerCommand (wildChild ^. name) (wildChild ^. name)

    verbose_runCommandErrors game command

prop_choosePlayerCommandErrorsWhenNotWildChildsTurn :: Game -> Property
prop_choosePlayerCommandErrorsWhenNotWildChildsTurn game =
    hasn't (stage . _WildChildsTurn) game
    ==> forAll (arbitraryChoosePlayerCommand game) $ verbose_runCommandErrors game . getBlind

prop_choosePlayerCommandErrorsWhenCallerNotWildChild :: GameAtWildChildsTurn -> Property
prop_choosePlayerCommandErrorsWhenCallerNotWildChild (GameAtWildChildsTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't wildChild)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = choosePlayerCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_choosePlayerCommandSetsRoleModel :: GameAtWildChildsTurn -> Property
prop_choosePlayerCommandSetsRoleModel (GameAtWildChildsTurn game) = do
    let wildChild = game ^?! players . wildChildren

    forAll (suchThat (arbitraryPlayer game) (wildChild /=)) $ \target -> do
        let command = choosePlayerCommand (wildChild ^. name) (target ^. name)
        let game'   = run_ (apply command) game

        fromJust (game' ^. roleModel) === target ^. name

prop_choosePlayersCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_choosePlayersCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryChoosePlayersCommand game) $ verbose_runCommandErrors game . getBlind

prop_choosePlayersCommandErrorsWhenCallerDoesNotExist :: GameAtScapegoatsTurn -> Player -> Property
prop_choosePlayersCommandErrorsWhenCallerDoesNotExist (GameAtScapegoatsTurn game) caller =
    forAll (NonEmpty <$> sublistOf (game ^.. players . traverse . alive)) $ \(NonEmpty targets) -> do
        let command = choosePlayersCommand (caller ^. name) (targets ^.. names)

        not (doesPlayerExist (caller ^. name) game)
            ==> verbose_runCommandErrors game command

prop_choosePlayersCommandErrorsWhenAnyTargetDoesNotExist :: GameAtScapegoatsTurn -> Player -> Property
prop_choosePlayersCommandErrorsWhenAnyTargetDoesNotExist (GameAtScapegoatsTurn game) target = do
    let scapegoat   = game ^?! players . scapegoats
    let command     = choosePlayersCommand (scapegoat ^. name) [target ^. name]

    not (doesPlayerExist (target ^. name) game)
        ==> verbose_runCommandErrors game command

prop_choosePlayersCommandErrorsWhenAnyTargetIsDead :: GameAtScapegoatsTurn -> Property
prop_choosePlayersCommandErrorsWhenAnyTargetIsDead (GameAtScapegoatsTurn game) = do
    let scapegoat = game ^?! players . scapegoats

    forAll (NonEmpty <$> sublistOf (game ^.. players . traverse . alive)) $ \(NonEmpty targets) ->
        forAll (elements targets) $ \target -> do
            let game'   = killPlayer (target ^. name) game
            let command = choosePlayersCommand (scapegoat ^. name) (targets ^.. names)

            verbose_runCommandErrors game' command

prop_choosePlayersCommandErrorsWhenNotScapegoatsTurn :: Game -> Property
prop_choosePlayersCommandErrorsWhenNotScapegoatsTurn game =
    hasn't (stage . _ScapegoatsTurn) game
    ==> forAll (arbitraryChoosePlayersCommand game) $ verbose_runCommandErrors game . getBlind

prop_choosePlayersCommandErrorsWhenCallerNotScapegoat :: GameAtScapegoatsTurn -> Property
prop_choosePlayersCommandErrorsWhenCallerNotScapegoat (GameAtScapegoatsTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't scapegoat)) $ \caller ->
    forAll (NonEmpty <$> sublistOf (game ^.. players . traverse . alive)) $ \(NonEmpty targets) -> do
        let command = choosePlayersCommand (caller ^. name) (targets ^.. names)

        verbose_runCommandErrors game command

prop_choosePlayersCommandSetsAllowedVoters :: GameAtScapegoatsTurn -> Property
prop_choosePlayersCommandSetsAllowedVoters (GameAtScapegoatsTurn game) = do
    let scapegoat = game ^?! players . scapegoats

    forAll (NonEmpty <$> sublistOf (game ^.. players . traverse . alive)) $ \(NonEmpty targets) -> do
        let command = choosePlayersCommand (scapegoat ^. name) (targets ^.. names)
        let game'   = run_ (apply command) game

        game' ^. allowedVoters === targets ^.. names

prop_choosePlayersCommandResetsScapegoatBlamed :: GameAtScapegoatsTurn -> Property
prop_choosePlayersCommandResetsScapegoatBlamed (GameAtScapegoatsTurn game) = do
    forAll (arbitraryChoosePlayersCommand game) $ \(Blind command) ->
        not $ run_ (apply command) game ^. scapegoatBlamed
