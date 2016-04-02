{-|
Module      : Game.Werewolf.Test.Command.Vote
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command.Vote (
    -- * Tests
    allVoteCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import qualified Data.Map as Map

import Game.Werewolf
import Game.Werewolf.Command.Villager as Villager
import Game.Werewolf.Command.Werewolf as Werewolf
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allVoteCommandTests :: [TestTree]
allVoteCommandTests =
    [ testProperty "werewolf vote command errors when game is over"             prop_werewolfVoteCommandErrorsWhenGameIsOver
    , testProperty "werewolf vote command errors when caller does not exist"    prop_werewolfVoteCommandErrorsWhenCallerDoesNotExist
    , testProperty "werewolf vote command errors when target does not exist"    prop_werewolfVoteCommandErrorsWhenTargetDoesNotExist
    , testProperty "werewolf vote command errors when caller is dead"           prop_werewolfVoteCommandErrorsWhenCallerIsDead
    , testProperty "werewolf vote command errors when target is dead"           prop_werewolfVoteCommandErrorsWhenTargetIsDead
    , testProperty "werewolf vote command errors when not werewolves turn"      prop_werewolfVoteCommandErrorsWhenNotWerewolvesTurn
    , testProperty "werewolf vote command errors when caller not werewolf"      prop_werewolfVoteCommandErrorsWhenCallerNotWerewolf
    , testProperty "werewolf vote command errors when caller has voted"         prop_werewolfVoteCommandErrorsWhenCallerHasVoted
    , testProperty "werewolf vote command errors when target werewolf"          prop_werewolfVoteCommandErrorsWhenTargetWerewolf
    , testProperty "werewolf vote command updates votes"                        prop_werewolfVoteCommandUpdatesVotes

    , testProperty "villager vote command errors when game is over"                     prop_villagerVoteCommandErrorsWhenGameIsOver
    , testProperty "villager vote command errors when caller does not exist"            prop_villagerVoteCommandErrorsWhenCallerDoesNotExist
    , testProperty "villager vote command errors when target does not exist"            prop_villagerVoteCommandErrorsWhenTargetDoesNotExist
    , testProperty "villager vote command errors when caller is dead"                   prop_villagerVoteCommandErrorsWhenCallerIsDead
    , testProperty "villager vote command errors when target is dead"                   prop_villagerVoteCommandErrorsWhenTargetIsDead
    , testProperty "villager vote command errors when not villages turn"                prop_villagerVoteCommandErrorsWhenNotVillagesTurn
    , testProperty "villager vote command errors when caller has voted"                 prop_villagerVoteCommandErrorsWhenCallerHasVoted
    , testProperty "villager vote command errors when caller is not in allowed voters"  prop_villagerVoteCommandErrorsWhenCallerIsNotInAllowedVoters
    , testProperty "villager vote command errors when caller is known jester"           prop_villagerVoteCommandErrorsWhenCallerIsKnownJester
    , testProperty "villager vote command updates votes"                                prop_villagerVoteCommandUpdatesVotes
    ]

prop_werewolfVoteCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_werewolfVoteCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryWerewolfVoteCommand game) $ verbose_runCommandErrors game . getBlind

prop_werewolfVoteCommandErrorsWhenCallerDoesNotExist :: GameAtWerewolvesTurn -> Player -> Property
prop_werewolfVoteCommandErrorsWhenCallerDoesNotExist (GameAtWerewolvesTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_werewolfVoteCommandErrorsWhenTargetDoesNotExist :: GameAtWerewolvesTurn -> Player -> Property
prop_werewolfVoteCommandErrorsWhenTargetDoesNotExist (GameAtWerewolvesTurn game) target =
    not (doesPlayerExist (target ^. name) game)
    ==> forAll (arbitraryWerewolf game) $ \caller -> do
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_werewolfVoteCommandErrorsWhenCallerIsDead :: GameAtWerewolvesTurn -> Property
prop_werewolfVoteCommandErrorsWhenCallerIsDead (GameAtWerewolvesTurn game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (caller ^. name) game
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_werewolfVoteCommandErrorsWhenTargetIsDead :: GameAtWerewolvesTurn -> Property
prop_werewolfVoteCommandErrorsWhenTargetIsDead (GameAtWerewolvesTurn game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_werewolfVoteCommandErrorsWhenNotWerewolvesTurn :: Game -> Property
prop_werewolfVoteCommandErrorsWhenNotWerewolvesTurn game =
    hasn't (stage . _WerewolvesTurn) game
    ==> forAll (arbitraryWerewolfVoteCommand game) $ verbose_runCommandErrors game . getBlind

prop_werewolfVoteCommandErrorsWhenCallerNotWerewolf :: GameAtWerewolvesTurn -> Property
prop_werewolfVoteCommandErrorsWhenCallerNotWerewolf (GameAtWerewolvesTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't werewolf)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_werewolfVoteCommandErrorsWhenCallerHasVoted :: GameWithDevourVotes -> Property
prop_werewolfVoteCommandErrorsWhenCallerHasVoted (GameWithDevourVotes game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (suchThat (arbitraryPlayer game) (isn't werewolf)) $ \target -> do
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_werewolfVoteCommandErrorsWhenTargetWerewolf :: GameAtWerewolvesTurn -> Property
prop_werewolfVoteCommandErrorsWhenTargetWerewolf (GameAtWerewolvesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryWerewolf game) $ \target ->
    verbose_runCommandErrors game (Werewolf.voteCommand (caller ^. name) (target ^. name))

prop_werewolfVoteCommandUpdatesVotes :: GameAtWerewolvesTurn -> Property
prop_werewolfVoteCommandUpdatesVotes (GameAtWerewolvesTurn game) =
    forAll (arbitraryWerewolfVoteCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        Map.size (game' ^. votes) == 1

prop_villagerVoteCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_villagerVoteCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryVillagerVoteCommand game) $ verbose_runCommandErrors game . getBlind

prop_villagerVoteCommandErrorsWhenCallerDoesNotExist :: GameAtVillagesTurn -> Player -> Property
prop_villagerVoteCommandErrorsWhenCallerDoesNotExist (GameAtVillagesTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_villagerVoteCommandErrorsWhenTargetDoesNotExist :: GameAtVillagesTurn -> Player -> Property
prop_villagerVoteCommandErrorsWhenTargetDoesNotExist (GameAtVillagesTurn game) target =
    not (doesPlayerExist (target ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \caller -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_villagerVoteCommandErrorsWhenCallerIsDead :: GameAtVillagesTurn -> Property
prop_villagerVoteCommandErrorsWhenCallerIsDead (GameAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (caller ^. name) game
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_villagerVoteCommandErrorsWhenTargetIsDead :: GameAtVillagesTurn -> Property
prop_villagerVoteCommandErrorsWhenTargetIsDead (GameAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_villagerVoteCommandErrorsWhenNotVillagesTurn :: Game -> Property
prop_villagerVoteCommandErrorsWhenNotVillagesTurn game =
    hasn't (stage . _VillagesTurn) game
    ==> forAll (arbitraryVillagerVoteCommand game) $ verbose_runCommandErrors game . getBlind

prop_villagerVoteCommandErrorsWhenCallerHasVoted :: GameWithLynchVotes -> Property
prop_villagerVoteCommandErrorsWhenCallerHasVoted (GameWithLynchVotes game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_villagerVoteCommandErrorsWhenCallerIsNotInAllowedVoters :: GameWithAllowedVoters -> Property
prop_villagerVoteCommandErrorsWhenCallerIsNotInAllowedVoters (GameWithAllowedVoters game) =
    forAll (suchThat (arbitraryPlayer game') (`notElem` getAllowedVoters game')) $ \caller ->
    forAll (arbitraryPlayer game') $ \target -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command
    where
        game' = run_ checkStage game

prop_villagerVoteCommandErrorsWhenCallerIsKnownJester :: GameWithJesterRevealedAtVillagesTurn -> Property
prop_villagerVoteCommandErrorsWhenCallerIsKnownJester (GameWithJesterRevealedAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command
    where
        caller = game ^?! players . jesters

prop_villagerVoteCommandUpdatesVotes :: GameAtVillagesTurn -> Property
prop_villagerVoteCommandUpdatesVotes (GameAtVillagesTurn game) =
    forAll (arbitraryVillagerVoteCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        Map.size (game' ^. votes) == 1
