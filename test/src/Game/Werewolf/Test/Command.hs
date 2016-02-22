{-|
Module      : Game.Werewolf.Test.Command
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command (
    -- * Tests
    allCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import           Data.Either.Extra
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.Text         (Text)
import qualified Data.Text         as T

import Game.Werewolf.Command
import Game.Werewolf.Engine         (checkStage)
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Role           hiding (name)
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allCommandTests :: [TestTree]
allCommandTests =
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

    , testProperty "heal command errors when game is over"          prop_healCommandErrorsWhenGameIsOver
    , testProperty "heal command errors when caller does not exist" prop_healCommandErrorsWhenCallerDoesNotExist
    , testProperty "heal command errors when caller is dead"        prop_healCommandErrorsWhenCallerIsDead
    , testProperty "heal command errors when no target is devoured" prop_healCommandErrorsWhenNoTargetIsDevoured
    , testProperty "heal command errors when not witch's turn"      prop_healCommandErrorsWhenNotWitchsTurn
    , testProperty "heal command errors when caller has healed"     prop_healCommandErrorsWhenCallerHasHealed
    , testProperty "heal command errors when caller not witch"      prop_healCommandErrorsWhenCallerNotWitch
    , testProperty "heal command sets heal"                         prop_healCommandSetsHeal
    , testProperty "heal command sets heal used"                    prop_healCommandSetsHealUsed

    , testProperty "pass command errors when game is over"          prop_passCommandErrorsWhenGameIsOver
    , testProperty "pass command errors when caller does not exist" prop_passCommandErrorsWhenCallerDoesNotExist
    , testProperty "pass command errors when caller is dead"        prop_passCommandErrorsWhenCallerIsDead
    , testProperty "pass command errors when not witch's turn"      prop_passCommandErrorsWhenNotWitchsTurn
    , testProperty "pass command updates passes"                    prop_passCommandUpdatesPasses

    , testProperty "poison command errors when game is over"            prop_poisonCommandErrorsWhenGameIsOver
    , testProperty "poison command errors when caller does not exist"   prop_poisonCommandErrorsWhenCallerDoesNotExist
    , testProperty "poison command errors when target does not exist"   prop_poisonCommandErrorsWhenTargetDoesNotExist
    , testProperty "poison command errors when caller is dead"          prop_poisonCommandErrorsWhenCallerIsDead
    , testProperty "poison command errors when target is dead"          prop_poisonCommandErrorsWhenTargetIsDead
    , testProperty "poison command errors when target is devoured"      prop_poisonCommandErrorsWhenTargetIsDevoured
    , testProperty "poison command errors when not witch's turn"        prop_poisonCommandErrorsWhenNotWitchsTurn
    , testProperty "poison command errors when caller has poisoned"     prop_poisonCommandErrorsWhenCallerHasPoisoned
    , testProperty "poison command errors when caller not witch"        prop_poisonCommandErrorsWhenCallerNotWitch
    -- TODO (hjw): implement this test case
    --, testProperty "poison command errors when caller devoured and not healed"   prop_poisonCommandErrorsWhenCallerDevouredAndNotHealed
    , testProperty "poison command sets poison"                         prop_poisonCommandSetsPoison
    , testProperty "poison command sets poison used"                    prop_poisonCommandSetsPoisonUsed

    , testProperty "protect command errors when game is over"               prop_protectCommandErrorsWhenGameIsOver
    , testProperty "protect command errors when caller does not exist"      prop_protectCommandErrorsWhenCallerDoesNotExist
    , testProperty "protect command errors when target does not exist"      prop_protectCommandErrorsWhenTargetDoesNotExist
    , testProperty "protect command errors when caller is dead"             prop_protectCommandErrorsWhenCallerIsDead
    , testProperty "protect command errors when target is dead"             prop_protectCommandErrorsWhenTargetIsDead
    , testProperty "protect command errors when not defender's turn"        prop_protectCommandErrorsWhenNotDefendersTurn
    , testProperty "protect command errors when caller not defender"        prop_protectCommandErrorsWhenCallerNotDefender
    , testProperty "protect command errors when target is prior protect"    prop_protectCommandErrorsWhenTargetIsPriorProtect
    , testProperty "protect command sets prior protect"                     prop_protectCommandSetsPriorProtect
    , testProperty "protect command sets protect"                           prop_protectCommandSetsProtect

    , testProperty "quit command errors when game is over"                              prop_quitCommandErrorsWhenGameIsOver
    , testProperty "quit command errors when caller does not exist"                     prop_quitCommandErrorsWhenCallerDoesNotExist
    , testProperty "quit command errors when caller is dead"                            prop_quitCommandErrorsWhenCallerIsDead
    , testProperty "quit command kills player"                                          prop_quitCommandKillsPlayer
    , testProperty "quit command clears allegiance chosen when caller is wolf-hound"    prop_quitCommandClearsAllegianceChosenWhenCallerIsWolfHound
    , testProperty "quit command clears heal when caller is witch"                      prop_quitCommandClearsHealWhenCallerIsWitch
    , testProperty "quit command clears heal used when caller is witch"                 prop_quitCommandClearsHealUsedWhenCallerIsWitch
    , testProperty "quit command clears poison when caller is witch"                    prop_quitCommandClearsPoisonWhenCallerIsWitch
    , testProperty "quit command clears poison used when caller is witch"               prop_quitCommandClearsPoisonUsedWhenCallerIsWitch
    , testProperty "quit command clears prior protect when caller is defender"          prop_quitCommandClearsPriorProtectWhenCallerIsDefender
    , testProperty "quit command clears protect when caller is defender"                prop_quitCommandClearsProtectWhenCallerIsDefender
    , testProperty "quit command clears player's devour vote"                           prop_quitCommandClearsPlayersDevourVote
    , testProperty "quit command clears player's lynch vote"                            prop_quitCommandClearsPlayersLynchVote
    , testProperty "quit command clears role model when caller is wild-child"           prop_quitCommandClearsRoleModelWhenCallerIsWildChild
    , testProperty "quit command sets angel's allegiance when caller is angel"          prop_quitCommandSetsAngelsAllegianceWhenCallerIsAngel

    , testProperty "see command errors when game is over"           prop_seeCommandErrorsWhenGameIsOver
    , testProperty "see command errors when caller does not exist"  prop_seeCommandErrorsWhenCallerDoesNotExist
    , testProperty "see command errors when target does not exist"  prop_seeCommandErrorsWhenTargetDoesNotExist
    , testProperty "see command errors when caller is dead"         prop_seeCommandErrorsWhenCallerIsDead
    , testProperty "see command errors when target is dead"         prop_seeCommandErrorsWhenTargetIsDead
    , testProperty "see command errors when not seer's turn"        prop_seeCommandErrorsWhenNotSeersTurn
    , testProperty "see command errors when caller not seer"        prop_seeCommandErrorsWhenCallerNotSeer
    , testProperty "see command sets see"                           prop_seeCommandSetsSee

    , testProperty "vote devour command errors when game is over"           prop_voteDevourCommandErrorsWhenGameIsOver
    , testProperty "vote devour command errors when caller does not exist"  prop_voteDevourCommandErrorsWhenCallerDoesNotExist
    , testProperty "vote devour command errors when target does not exist"  prop_voteDevourCommandErrorsWhenTargetDoesNotExist
    , testProperty "vote devour command errors when caller is dead"         prop_voteDevourCommandErrorsWhenCallerIsDead
    , testProperty "vote devour command errors when target is dead"         prop_voteDevourCommandErrorsWhenTargetIsDead
    , testProperty "vote devour command errors when not werewolves turn"    prop_voteDevourCommandErrorsWhenNotWerewolvesTurn
    , testProperty "vote devour command errors when caller not werewolf"    prop_voteDevourCommandErrorsWhenCallerNotWerewolf
    , testProperty "vote devour command errors when caller has voted"       prop_voteDevourCommandErrorsWhenCallerHasVoted
    , testProperty "vote devour command errors when target werewolf"        prop_voteDevourCommandErrorsWhenTargetWerewolf
    , testProperty "vote devour command updates votes"                      prop_voteDevourCommandUpdatesVotes

    , testProperty "vote lynch command errors when game is over"                    prop_voteLynchCommandErrorsWhenGameIsOver
    , testProperty "vote lynch command errors when caller does not exist"           prop_voteLynchCommandErrorsWhenCallerDoesNotExist
    , testProperty "vote lynch command errors when target does not exist"           prop_voteLynchCommandErrorsWhenTargetDoesNotExist
    , testProperty "vote lynch command errors when caller is dead"                  prop_voteLynchCommandErrorsWhenCallerIsDead
    , testProperty "vote lynch command errors when target is dead"                  prop_voteLynchCommandErrorsWhenTargetIsDead
    , testProperty "vote lynch command errors when not villages turn"               prop_voteLynchCommandErrorsWhenNotVillagesTurn
    , testProperty "vote lynch command errors when caller has voted"                prop_voteLynchCommandErrorsWhenCallerHasVoted
    , testProperty "vote lynch command errors when caller is not in allowed voters" prop_voteLynchCommandErrorsWhenCallerIsNotInAllowedVoters
    , testProperty "vote lynch command errors when caller is known village idiot"   prop_voteLynchCommandErrorsWhenCallerIsKnownVillageIdiot
    , testProperty "vote lynch command errors when target is known village idiot"   prop_voteLynchCommandErrorsWhenTargetIsKnownVillageIdiot
    , testProperty "vote lynch command updates votes"                               prop_voteLynchCommandUpdatesVotes
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

prop_healCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_healCommandErrorsWhenGameIsOver (GameAtGameOver game) = do
    let witch   = game ^?! players . witches
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerDoesNotExist :: GameWithDevourEvent -> Player -> Property
prop_healCommandErrorsWhenCallerDoesNotExist (GameWithDevourEvent game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (healCommand (caller ^. name))

prop_healCommandErrorsWhenCallerIsDead :: GameWithDevourEvent -> Property
prop_healCommandErrorsWhenCallerIsDead (GameWithDevourEvent game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = healCommand (caller ^. name)

        verbose_runCommandErrors game' command

prop_healCommandErrorsWhenNoTargetIsDevoured :: GameAtWitchsTurn -> Property
prop_healCommandErrorsWhenNoTargetIsDevoured (GameAtWitchsTurn game) = do
    let witch   = game ^?! players . witches
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_healCommandErrorsWhenNotWitchsTurn game = do
    let witch   = game ^?! players . witches
    let command = healCommand $ witch ^. name

    hasn't (stage . _WitchsTurn) game
        ==> verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerHasHealed :: GameWithHeal -> Property
prop_healCommandErrorsWhenCallerHasHealed (GameWithHeal game) = do
    let witch   = game ^?! players . witches
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerNotWitch :: GameWithDevourEvent -> Property
prop_healCommandErrorsWhenCallerNotWitch (GameWithDevourEvent game) =
    forAll (suchThat (arbitraryPlayer game) (isn't witch)) $ \caller -> do
        let command = healCommand (caller ^. name)

        verbose_runCommandErrors game command

prop_healCommandSetsHeal :: GameWithDevourEvent -> Property
prop_healCommandSetsHeal (GameWithDevourEvent game) =
    forAll (arbitraryHealCommand game) $ \(Blind command) ->
        (run_ (apply command) game) ^. heal

prop_healCommandSetsHealUsed :: GameWithDevourEvent -> Property
prop_healCommandSetsHealUsed (GameWithDevourEvent game) =
    forAll (arbitraryHealCommand game) $ \(Blind command) ->
        (run_ (apply command) game) ^. healUsed

prop_passCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_passCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_passCommandErrorsWhenCallerDoesNotExist :: GameAtWitchsTurn -> Player -> Property
prop_passCommandErrorsWhenCallerDoesNotExist (GameAtWitchsTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (passCommand (caller ^. name))

prop_passCommandErrorsWhenCallerIsDead :: GameAtWitchsTurn -> Property
prop_passCommandErrorsWhenCallerIsDead (GameAtWitchsTurn game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = passCommand (caller ^. name)

        verbose_runCommandErrors game' command

prop_passCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_passCommandErrorsWhenNotWitchsTurn game =
    hasn't (stage . _WitchsTurn) game
    ==> forAll (arbitraryPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_passCommandUpdatesPasses :: GameAtWitchsTurn -> Property
prop_passCommandUpdatesPasses (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        length (game' ^. passes) == 1

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

    isNothing $ run_ (apply command) game ^. allegianceChosen

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

prop_quitCommandClearsPriorProtectWhenCallerIsDefender :: GameWithProtect -> Bool
prop_quitCommandClearsPriorProtectWhenCallerIsDefender (GameWithProtect game) = do
    let defender    = game ^?! players . defenders
    let command     = quitCommand (defender ^. name)

    isNothing $ run_ (apply command) game ^. priorProtect

prop_quitCommandClearsProtectWhenCallerIsDefender :: GameWithProtect -> Bool
prop_quitCommandClearsProtectWhenCallerIsDefender (GameWithProtect game) = do
    let defender    = game ^?! players . defenders
    let command     = quitCommand (defender ^. name)

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

prop_quitCommandClearsRoleModelWhenCallerIsWildChild :: GameWithRoleModel -> Bool
prop_quitCommandClearsRoleModelWhenCallerIsWildChild (GameWithRoleModel game) = do
    let wildChild   = game ^?! players . wildChildren
    let command     = quitCommand (wildChild ^. name)

    isNothing $ run_ (apply command) game ^. roleModel

prop_quitCommandSetsAngelsAllegianceWhenCallerIsAngel :: Game -> Bool
prop_quitCommandSetsAngelsAllegianceWhenCallerIsAngel game = do
    let angelsName  = game ^?! players . angels . name
    let command     = quitCommand angelsName
    let game'       = run_ (apply command) game

    is villager $ game' ^?! players . angels

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

prop_voteDevourCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_voteDevourCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryVoteDevourCommand game) $ verbose_runCommandErrors game . getBlind

prop_voteDevourCommandErrorsWhenCallerDoesNotExist :: GameAtWerewolvesTurn -> Player -> Property
prop_voteDevourCommandErrorsWhenCallerDoesNotExist (GameAtWerewolvesTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = voteDevourCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteDevourCommandErrorsWhenTargetDoesNotExist :: GameAtWerewolvesTurn -> Player -> Property
prop_voteDevourCommandErrorsWhenTargetDoesNotExist (GameAtWerewolvesTurn game) target =
    not (doesPlayerExist (target ^. name) game)
    ==> forAll (arbitraryWerewolf game) $ \caller -> do
        let command = voteDevourCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteDevourCommandErrorsWhenCallerIsDead :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandErrorsWhenCallerIsDead (GameAtWerewolvesTurn game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (caller ^. name) game
        let command = voteDevourCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_voteDevourCommandErrorsWhenTargetIsDead :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandErrorsWhenTargetIsDead (GameAtWerewolvesTurn game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = voteDevourCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_voteDevourCommandErrorsWhenNotWerewolvesTurn :: Game -> Property
prop_voteDevourCommandErrorsWhenNotWerewolvesTurn game =
    hasn't (stage . _WerewolvesTurn) game
    ==> forAll (arbitraryVoteDevourCommand game) $ verbose_runCommandErrors game . getBlind

prop_voteDevourCommandErrorsWhenCallerNotWerewolf :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandErrorsWhenCallerNotWerewolf (GameAtWerewolvesTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't werewolf)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = voteDevourCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteDevourCommandErrorsWhenCallerHasVoted :: GameWithDevourVotes -> Property
prop_voteDevourCommandErrorsWhenCallerHasVoted (GameWithDevourVotes game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (suchThat (arbitraryPlayer game) (isn't werewolf)) $ \target -> do
        let command = voteDevourCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteDevourCommandErrorsWhenTargetWerewolf :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandErrorsWhenTargetWerewolf (GameAtWerewolvesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryWerewolf game) $ \target ->
    verbose_runCommandErrors game (voteDevourCommand (caller ^. name) (target ^. name))

prop_voteDevourCommandUpdatesVotes :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandUpdatesVotes (GameAtWerewolvesTurn game) =
    forAll (arbitraryVoteDevourCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        Map.size (game' ^. votes) == 1

prop_voteLynchCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_voteLynchCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryVoteLynchCommand game) $ verbose_runCommandErrors game . getBlind

prop_voteLynchCommandErrorsWhenCallerDoesNotExist :: GameAtVillagesTurn -> Player -> Property
prop_voteLynchCommandErrorsWhenCallerDoesNotExist (GameAtVillagesTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = voteLynchCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteLynchCommandErrorsWhenTargetDoesNotExist :: GameAtVillagesTurn -> Player -> Property
prop_voteLynchCommandErrorsWhenTargetDoesNotExist (GameAtVillagesTurn game) target =
    not (doesPlayerExist (target ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \caller -> do
        let command = voteLynchCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteLynchCommandErrorsWhenCallerIsDead :: GameAtVillagesTurn -> Property
prop_voteLynchCommandErrorsWhenCallerIsDead (GameAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (caller ^. name) game
        let command = voteLynchCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_voteLynchCommandErrorsWhenTargetIsDead :: GameAtVillagesTurn -> Property
prop_voteLynchCommandErrorsWhenTargetIsDead (GameAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = voteLynchCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_voteLynchCommandErrorsWhenNotVillagesTurn :: Game -> Property
prop_voteLynchCommandErrorsWhenNotVillagesTurn game =
    hasn't (stage . _VillagesTurn) game
    ==> forAll (arbitraryVoteLynchCommand game) $ verbose_runCommandErrors game . getBlind

prop_voteLynchCommandErrorsWhenCallerHasVoted :: GameWithLynchVotes -> Property
prop_voteLynchCommandErrorsWhenCallerHasVoted (GameWithLynchVotes game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = voteLynchCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteLynchCommandErrorsWhenCallerIsNotInAllowedVoters :: GameWithAllowedVoters -> Property
prop_voteLynchCommandErrorsWhenCallerIsNotInAllowedVoters (GameWithAllowedVoters game) =
    forAll (suchThat (arbitraryPlayer game') (`notElem` getAllowedVoters game')) $ \caller ->
    forAll (arbitraryPlayer game') $ \target -> do
        let command = voteLynchCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command
    where
        game' = run_ checkStage game

prop_voteLynchCommandErrorsWhenCallerIsKnownVillageIdiot :: GameWithVillageIdiotRevealedAtVillagesTurn -> Property
prop_voteLynchCommandErrorsWhenCallerIsKnownVillageIdiot (GameWithVillageIdiotRevealedAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \target -> do
        let command = voteLynchCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command
    where
        caller = game ^?! players . villageIdiots

prop_voteLynchCommandErrorsWhenTargetIsKnownVillageIdiot :: GameWithVillageIdiotRevealedAtVillagesTurn -> Property
prop_voteLynchCommandErrorsWhenTargetIsKnownVillageIdiot (GameWithVillageIdiotRevealedAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let command = voteLynchCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command
    where
        target = game ^?! players . villageIdiots

prop_voteLynchCommandUpdatesVotes :: GameAtVillagesTurn -> Property
prop_voteLynchCommandUpdatesVotes (GameAtVillagesTurn game) =
    forAll (arbitraryVoteLynchCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        Map.size (game' ^. votes) == 1

verbose_runCommandErrors :: Game -> Command -> Property
verbose_runCommandErrors game command = whenFail (mapM_ putStrLn data_) (isLeft result)
    where
        result  = run (apply command) game
        data_   = [show game, show $ fromRight result]
