{-|
Module      : Game.Werewolf.Test.Command
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Game.Werewolf.Test.Command (
    -- * killVoteCommand
    prop_killVoteCommandErrorsWhenGameIsOver, prop_killVoteCommandErrorsWhenCallerDoesNotExist,
    prop_killVoteCommandErrorsWhenTargetDoesNotExist, prop_killVoteCommandErrorsWhenCallerIsDead,
    prop_killVoteCommandErrorsWhenTargetIsDead, prop_killVoteCommandErrorsWhenNotWerewolvesTurn,
    prop_killVoteCommandErrorsWhenCallerNotWerewolf, prop_killVoteCommandErrorsWhenCallerHasVoted,
    prop_killVoteCommandErrorsWhenTargetWerewolf, prop_killVoteCommandUpdatesVotes,

    -- * lynchVoteCommand
    prop_lynchVoteCommandErrorsWhenGameIsOver, prop_lynchVoteCommandErrorsWhenCallerDoesNotExist,
    prop_lynchVoteCommandErrorsWhenTargetDoesNotExist, prop_lynchVoteCommandErrorsWhenCallerIsDead,
    prop_lynchVoteCommandErrorsWhenTargetIsDead, prop_lynchVoteCommandErrorsWhenNotVillagersTurn,
    prop_lynchVoteCommandErrorsWhenCallerHasVoted, prop_lynchVoteCommandUpdatesVotes,

    -- * quitCommand
    prop_quitCommandErrorsWhenGameIsOver, prop_quitCommandErrorsWhenCallerDoesNotExist,
    prop_quitCommandErrorsWhenCallerIsDead, prop_quitCommandKillsPlayer,
    prop_quitCommandClearsPlayersKillVote, prop_quitCommandClearsPlayersLynchVote,
    prop_quitCommandClearsPlayersSee,

    -- * seeCommand
    prop_seeCommandErrorsWhenGameIsOver, prop_seeCommandErrorsWhenCallerDoesNotExist,
    prop_seeCommandErrorsWhenTargetDoesNotExist, prop_seeCommandErrorsWhenCallerIsDead,
    prop_seeCommandErrorsWhenTargetIsDead, prop_seeCommandErrorsWhenNotSeersTurn,
    prop_seeCommandErrorsWhenCallerNotSeer, prop_seeCommandErrorsWhenCallerHasSeen,
    prop_seeCommandUpdatesSees,
) where

import Control.Lens hiding (elements)

import Data.Either.Extra
import Data.Map          as Map

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Test.Arbitrary

import Test.QuickCheck

prop_killVoteCommandErrorsWhenGameIsOver :: Game -> Property
prop_killVoteCommandErrorsWhenGameIsOver game =
    isGameOver game
    ==> forAll (arbitraryKillVoteCommand game) $ verbose_runCommandErrors game

prop_killVoteCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_killVoteCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (killVoteCommand (caller ^. name) (target ^. name))

prop_killVoteCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_killVoteCommandErrorsWhenTargetDoesNotExist game target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitraryWerewolf game) $ \caller ->
        verbose_runCommandErrors game (killVoteCommand (caller ^. name) (target ^. name))

prop_killVoteCommandErrorsWhenCallerIsDead :: Game -> Property
prop_killVoteCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game caller) (killVoteCommand (caller ^. name) (target ^. name))

prop_killVoteCommandErrorsWhenTargetIsDead :: Game -> Property
prop_killVoteCommandErrorsWhenTargetIsDead game =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game target) (killVoteCommand (caller ^. name) (target ^. name))

prop_killVoteCommandErrorsWhenNotWerewolvesTurn :: Game -> Property
prop_killVoteCommandErrorsWhenNotWerewolvesTurn game =
    not (isWerewolvesTurn game)
    ==> forAll (arbitraryKillVoteCommand game) $ verbose_runCommandErrors game

prop_killVoteCommandErrorsWhenCallerNotWerewolf :: Game -> Property
prop_killVoteCommandErrorsWhenCallerNotWerewolf game =
    forAll (arbitraryPlayer game) $ \caller -> not (isWerewolf caller)
    ==> forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (killVoteCommand (caller ^. name) (target ^. name))

prop_killVoteCommandErrorsWhenCallerHasVoted :: Game -> Property
prop_killVoteCommandErrorsWhenCallerHasVoted game =
    isWerewolvesTurn game ==>
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    not (isWerewolf target)
    ==> let command = killVoteCommand (caller ^. name) (target ^. name)
        in verbose_runCommandErrors (run_ (apply command) game) command

prop_killVoteCommandErrorsWhenTargetWerewolf :: Game -> Property
prop_killVoteCommandErrorsWhenTargetWerewolf game =
    forAll (arbitraryPlayer game) $ \target -> isWerewolf target
    ==> forAll (arbitraryPlayer game) $ \caller ->
        verbose_runCommandErrors game (killVoteCommand (caller ^. name) (target ^. name))

prop_killVoteCommandUpdatesVotes :: Game -> Property
prop_killVoteCommandUpdatesVotes game =
    isWerewolvesTurn game
    ==> forAll (arbitraryKillVoteCommand game) $ \command ->
        Map.size (run_ (apply command) game ^. votes) == 1

prop_lynchVoteCommandErrorsWhenGameIsOver :: Game -> Property
prop_lynchVoteCommandErrorsWhenGameIsOver game =
    isGameOver game
    ==> forAll (arbitraryLynchVoteCommand game) $ verbose_runCommandErrors game

prop_lynchVoteCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_lynchVoteCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (lynchVoteCommand (caller ^. name) (target ^. name))

prop_lynchVoteCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_lynchVoteCommandErrorsWhenTargetDoesNotExist game target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \caller ->
        verbose_runCommandErrors game (lynchVoteCommand (caller ^. name) (target ^. name))

prop_lynchVoteCommandErrorsWhenCallerIsDead :: Game -> Property
prop_lynchVoteCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game caller) (lynchVoteCommand (caller ^. name) (target ^. name))

prop_lynchVoteCommandErrorsWhenTargetIsDead :: Game -> Property
prop_lynchVoteCommandErrorsWhenTargetIsDead game =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game target) (lynchVoteCommand (caller ^. name) (target ^. name))

prop_lynchVoteCommandErrorsWhenNotVillagersTurn :: Game -> Property
prop_lynchVoteCommandErrorsWhenNotVillagersTurn game =
    not (isVillagersTurn game)
    ==> forAll (arbitraryLynchVoteCommand game) $ verbose_runCommandErrors game

prop_lynchVoteCommandErrorsWhenCallerHasVoted :: Game -> Property
prop_lynchVoteCommandErrorsWhenCallerHasVoted game =
    isVillagersTurn game ==>
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    let command = lynchVoteCommand (caller ^. name) (target ^. name)
        in verbose_runCommandErrors (run_ (apply command) game) command

prop_lynchVoteCommandUpdatesVotes :: Game -> Property
prop_lynchVoteCommandUpdatesVotes game =
    isVillagersTurn game ==>
    forAll (arbitraryLynchVoteCommand game) $ \command ->
    Map.size (run_ (apply command) game ^. votes) == 1

prop_quitCommandErrorsWhenGameIsOver :: Game -> Property
prop_quitCommandErrorsWhenGameIsOver game =
    isGameOver game
    ==> forAll (arbitraryQuitCommand game) $ verbose_runCommandErrors game

prop_quitCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_quitCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> verbose_runCommandErrors game (quitCommand $ caller ^. name)

prop_quitCommandErrorsWhenCallerIsDead :: Game -> Property
prop_quitCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller ->
    verbose_runCommandErrors (killPlayer game caller) (quitCommand $ caller ^. name)

prop_quitCommandKillsPlayer :: Game -> Property
prop_quitCommandKillsPlayer game =
    not (isGameOver game)
    ==> forAll (arbitraryQuitCommand game) $ \command ->
        length (filterDead $ run_ (apply command) game ^. players) == 1

prop_quitCommandClearsPlayersKillVote :: Game -> Property
prop_quitCommandClearsPlayersKillVote game =
    forAll (arbitraryWerewolf game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    not (isWerewolf target)
    ==> let game'' = run_ (apply $ killVoteCommand (caller ^. name) (target ^. name)) game'
        in Map.null $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. votes
    where
        game' = game { _turn = Werewolves }

prop_quitCommandClearsPlayersLynchVote :: Game -> Property
prop_quitCommandClearsPlayersLynchVote game =
    forAll (arbitraryWerewolf game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    let game'' = run_ (apply $ lynchVoteCommand (caller ^. name) (target ^. name)) game'
        in Map.null $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. votes
    where
        game' = game { _turn = Villagers }

prop_quitCommandClearsPlayersSee :: Game -> Property
prop_quitCommandClearsPlayersSee game =
    forAll (arbitrarySeer game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    let game'' = run_ (apply $ seeCommand (caller ^. name) (target ^. name)) game'
        in Map.null $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. votes
    where
        game' = game { _turn = Seers }

prop_seeCommandErrorsWhenGameIsOver :: Game -> Property
prop_seeCommandErrorsWhenGameIsOver game =
    isGameOver game
    ==> forAll (arbitrarySeeCommand game) $ verbose_runCommandErrors game

prop_seeCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_seeCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (seeCommand (caller ^. name) (target ^. name))

prop_seeCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_seeCommandErrorsWhenTargetDoesNotExist game target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitrarySeer game) $ \caller ->
        verbose_runCommandErrors game (seeCommand (caller ^. name) (target ^. name))

prop_seeCommandErrorsWhenCallerIsDead :: Game -> Property
prop_seeCommandErrorsWhenCallerIsDead game =
    forAll (arbitrarySeer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game caller) (seeCommand (caller ^. name) (target ^. name))

prop_seeCommandErrorsWhenTargetIsDead :: Game -> Property
prop_seeCommandErrorsWhenTargetIsDead game =
    forAll (arbitrarySeer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game target) (seeCommand (caller ^. name) (target ^. name))

prop_seeCommandErrorsWhenNotSeersTurn :: Game -> Property
prop_seeCommandErrorsWhenNotSeersTurn game =
    not (isSeersTurn game)
    ==> forAll (arbitrarySeeCommand game) $ verbose_runCommandErrors game

prop_seeCommandErrorsWhenCallerNotSeer :: Game -> Property
prop_seeCommandErrorsWhenCallerNotSeer game =
    forAll (arbitraryPlayer game) $ \caller ->
    not (isSeer caller)
    ==> forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (seeCommand (caller ^. name) (target ^. name))

prop_seeCommandErrorsWhenCallerHasSeen :: Game -> Property
prop_seeCommandErrorsWhenCallerHasSeen game =
    isSeersTurn game
    ==> forAll (arbitrarySeer game) $ \caller ->
        forAll (arbitraryPlayer game) $ \target ->
        let command = seeCommand (caller ^. name) (target ^. name)
            in verbose_runCommandErrors (run_ (apply command) game) command

prop_seeCommandUpdatesSees :: Game -> Property
prop_seeCommandUpdatesSees game =
    isSeersTurn game
    ==> forAll (arbitrarySeeCommand game) $ \command ->
        Map.size (run_ (apply command) game ^. sees) == 1

verbose_runCommandErrors :: Game -> Command -> Property
verbose_runCommandErrors game command = whenFail (mapM_ putStrLn [show game, show command, show . fromRight $ run (apply command) game]) (isLeft $ run (apply command) game)
