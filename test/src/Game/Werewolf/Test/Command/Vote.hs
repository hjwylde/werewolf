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
    [ testProperty "vote devour command errors when game is over"           prop_voteDevourCommandErrorsWhenGameIsOver
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
    , testProperty "vote lynch command updates votes"                               prop_voteLynchCommandUpdatesVotes
    ]

prop_voteDevourCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_voteDevourCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryVoteDevourCommand game) $ verbose_runCommandErrors game . getBlind

prop_voteDevourCommandErrorsWhenCallerDoesNotExist :: GameAtWerewolvesTurn -> Player -> Property
prop_voteDevourCommandErrorsWhenCallerDoesNotExist (GameAtWerewolvesTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteDevourCommandErrorsWhenTargetDoesNotExist :: GameAtWerewolvesTurn -> Player -> Property
prop_voteDevourCommandErrorsWhenTargetDoesNotExist (GameAtWerewolvesTurn game) target =
    not (doesPlayerExist (target ^. name) game)
    ==> forAll (arbitraryWerewolf game) $ \caller -> do
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteDevourCommandErrorsWhenCallerIsDead :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandErrorsWhenCallerIsDead (GameAtWerewolvesTurn game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (caller ^. name) game
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_voteDevourCommandErrorsWhenTargetIsDead :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandErrorsWhenTargetIsDead (GameAtWerewolvesTurn game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_voteDevourCommandErrorsWhenNotWerewolvesTurn :: Game -> Property
prop_voteDevourCommandErrorsWhenNotWerewolvesTurn game =
    hasn't (stage . _WerewolvesTurn) game
    ==> forAll (arbitraryVoteDevourCommand game) $ verbose_runCommandErrors game . getBlind

prop_voteDevourCommandErrorsWhenCallerNotWerewolf :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandErrorsWhenCallerNotWerewolf (GameAtWerewolvesTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't werewolf)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteDevourCommandErrorsWhenCallerHasVoted :: GameWithDevourVotes -> Property
prop_voteDevourCommandErrorsWhenCallerHasVoted (GameWithDevourVotes game) =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (suchThat (arbitraryPlayer game) (isn't werewolf)) $ \target -> do
        let command = Werewolf.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteDevourCommandErrorsWhenTargetWerewolf :: GameAtWerewolvesTurn -> Property
prop_voteDevourCommandErrorsWhenTargetWerewolf (GameAtWerewolvesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryWerewolf game) $ \target ->
    verbose_runCommandErrors game (Werewolf.voteCommand (caller ^. name) (target ^. name))

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
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteLynchCommandErrorsWhenTargetDoesNotExist :: GameAtVillagesTurn -> Player -> Property
prop_voteLynchCommandErrorsWhenTargetDoesNotExist (GameAtVillagesTurn game) target =
    not (doesPlayerExist (target ^. name) game)
    ==> forAll (arbitraryPlayer game) $ \caller -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteLynchCommandErrorsWhenCallerIsDead :: GameAtVillagesTurn -> Property
prop_voteLynchCommandErrorsWhenCallerIsDead (GameAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (caller ^. name) game
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_voteLynchCommandErrorsWhenTargetIsDead :: GameAtVillagesTurn -> Property
prop_voteLynchCommandErrorsWhenTargetIsDead (GameAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_voteLynchCommandErrorsWhenNotVillagesTurn :: Game -> Property
prop_voteLynchCommandErrorsWhenNotVillagesTurn game =
    hasn't (stage . _VillagesTurn) game
    ==> forAll (arbitraryVoteLynchCommand game) $ verbose_runCommandErrors game . getBlind

prop_voteLynchCommandErrorsWhenCallerHasVoted :: GameWithLynchVotes -> Property
prop_voteLynchCommandErrorsWhenCallerHasVoted (GameWithLynchVotes game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_voteLynchCommandErrorsWhenCallerIsNotInAllowedVoters :: GameWithAllowedVoters -> Property
prop_voteLynchCommandErrorsWhenCallerIsNotInAllowedVoters (GameWithAllowedVoters game) =
    forAll (suchThat (arbitraryPlayer game') (`notElem` getAllowedVoters game')) $ \caller ->
    forAll (arbitraryPlayer game') $ \target -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command
    where
        game' = run_ checkStage game

prop_voteLynchCommandErrorsWhenCallerIsKnownVillageIdiot :: GameWithVillageIdiotRevealedAtVillagesTurn -> Property
prop_voteLynchCommandErrorsWhenCallerIsKnownVillageIdiot (GameWithVillageIdiotRevealedAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \target -> do
        let command = Villager.voteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command
    where
        caller = game ^?! players . villageIdiots

prop_voteLynchCommandUpdatesVotes :: GameAtVillagesTurn -> Property
prop_voteLynchCommandUpdatesVotes (GameAtVillagesTurn game) =
    forAll (arbitraryVoteLynchCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        Map.size (game' ^. votes) == 1
