{-|
Module      : Game.Werewolf.Test.Command
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Game.Werewolf.Test.Command (
    -- * devourVoteCommand
    prop_devourVoteCommandErrorsWhenGameIsOver, prop_devourVoteCommandErrorsWhenCallerDoesNotExist,
    prop_devourVoteCommandErrorsWhenTargetDoesNotExist,
    prop_devourVoteCommandErrorsWhenCallerIsDead, prop_devourVoteCommandErrorsWhenTargetIsDead,
    prop_devourVoteCommandErrorsWhenNotWerewolvesTurn,
    prop_devourVoteCommandErrorsWhenCallerNotWerewolf,
    prop_devourVoteCommandErrorsWhenCallerHasVoted, prop_devourVoteCommandErrorsWhenTargetWerewolf,
    prop_devourVoteCommandUpdatesVotes,

    -- * lynchVoteCommand
    prop_lynchVoteCommandErrorsWhenGameIsOver, prop_lynchVoteCommandErrorsWhenCallerDoesNotExist,
    prop_lynchVoteCommandErrorsWhenTargetDoesNotExist, prop_lynchVoteCommandErrorsWhenCallerIsDead,
    prop_lynchVoteCommandErrorsWhenTargetIsDead, prop_lynchVoteCommandErrorsWhenNotVillagesTurn,
    prop_lynchVoteCommandErrorsWhenCallerHasVoted, prop_lynchVoteCommandUpdatesVotes,

    -- * quitCommand
    prop_quitCommandErrorsWhenGameIsOver, prop_quitCommandErrorsWhenCallerDoesNotExist,
    prop_quitCommandErrorsWhenCallerIsDead, prop_quitCommandKillsPlayer,
    prop_quitCommandClearsPlayersDevourVote, prop_quitCommandClearsPlayersLynchVote,
    prop_quitCommandClearsPlayersSee,

    -- * seeCommand
    prop_seeCommandErrorsWhenGameIsOver, prop_seeCommandErrorsWhenCallerDoesNotExist,
    prop_seeCommandErrorsWhenTargetDoesNotExist, prop_seeCommandErrorsWhenCallerIsDead,
    prop_seeCommandErrorsWhenTargetIsDead, prop_seeCommandErrorsWhenNotSeersTurn,
    prop_seeCommandErrorsWhenCallerNotSeer, prop_seeCommandSetsSee,
) where

import Control.Lens hiding (elements)

import Data.Either.Extra
import Data.Map          as Map
import Data.Maybe

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Test.Arbitrary

import Test.QuickCheck

prop_devourVoteCommandErrorsWhenGameIsOver :: Game -> Property
prop_devourVoteCommandErrorsWhenGameIsOver game =
    isGameOver game
    ==> forAll (arbitraryDevourVoteCommand game) $ verbose_runCommandErrors game

prop_devourVoteCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_devourVoteCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_devourVoteCommandErrorsWhenTargetDoesNotExist game target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitraryWerewolf game) $ \caller ->
        verbose_runCommandErrors game (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandErrorsWhenCallerIsDead :: Game -> Property
prop_devourVoteCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game caller) (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandErrorsWhenTargetIsDead :: Game -> Property
prop_devourVoteCommandErrorsWhenTargetIsDead game =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game target) (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandErrorsWhenNotWerewolvesTurn :: Game -> Property
prop_devourVoteCommandErrorsWhenNotWerewolvesTurn game =
    not (isWerewolvesTurn game)
    ==> forAll (arbitraryDevourVoteCommand game) $ verbose_runCommandErrors game

prop_devourVoteCommandErrorsWhenCallerNotWerewolf :: Game -> Property
prop_devourVoteCommandErrorsWhenCallerNotWerewolf game =
    forAll (arbitraryPlayer game) $ \caller -> not (isWerewolf caller)
    ==> forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandErrorsWhenCallerHasVoted :: Game -> Property
prop_devourVoteCommandErrorsWhenCallerHasVoted game =
    isWerewolvesTurn game ==>
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    not (isWerewolf target)
    ==> let command = devourVoteCommand (caller ^. name) (target ^. name)
        in verbose_runCommandErrors (run_ (apply command) game) command

prop_devourVoteCommandErrorsWhenTargetWerewolf :: Game -> Property
prop_devourVoteCommandErrorsWhenTargetWerewolf game =
    forAll (arbitraryPlayer game) $ \target -> isWerewolf target
    ==> forAll (arbitraryPlayer game) $ \caller ->
        verbose_runCommandErrors game (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandUpdatesVotes :: Game -> Property
prop_devourVoteCommandUpdatesVotes game =
    isWerewolvesTurn game
    ==> forAll (arbitraryDevourVoteCommand game) $ \command ->
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

prop_lynchVoteCommandErrorsWhenNotVillagesTurn :: Game -> Property
prop_lynchVoteCommandErrorsWhenNotVillagesTurn game =
    not (isVillagesTurn game)
    ==> forAll (arbitraryLynchVoteCommand game) $ verbose_runCommandErrors game

prop_lynchVoteCommandErrorsWhenCallerHasVoted :: Game -> Property
prop_lynchVoteCommandErrorsWhenCallerHasVoted game =
    isVillagesTurn game ==>
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    let command = lynchVoteCommand (caller ^. name) (target ^. name)
        in verbose_runCommandErrors (run_ (apply command) game) command

prop_lynchVoteCommandUpdatesVotes :: Game -> Property
prop_lynchVoteCommandUpdatesVotes game =
    isVillagesTurn game ==>
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

prop_quitCommandClearsPlayersDevourVote :: Game -> Property
prop_quitCommandClearsPlayersDevourVote game =
    forAll (arbitraryWerewolf game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    not (isWerewolf target)
    ==> let game'' = run_ (apply $ devourVoteCommand (caller ^. name) (target ^. name)) game'
        in Map.null $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. votes
    where
        game' = game { _stage = WerewolvesTurn }

prop_quitCommandClearsPlayersLynchVote :: Game -> Property
prop_quitCommandClearsPlayersLynchVote game =
    forAll (arbitraryWerewolf game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    let game'' = run_ (apply $ lynchVoteCommand (caller ^. name) (target ^. name)) game'
        in Map.null $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. votes
    where
        game' = game { _stage = VillagesTurn }

prop_quitCommandClearsPlayersSee :: Game -> Property
prop_quitCommandClearsPlayersSee game =
    forAll (arbitrarySeer game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    let game'' = run_ (apply $ seeCommand (caller ^. name) (target ^. name)) game'
        in Map.null $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. votes
    where
        game' = game { _stage = SeersTurn }

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

prop_seeCommandSetsSee :: Game -> Property
prop_seeCommandSetsSee game =
    isSeersTurn game
    ==> forAll (arbitrarySeeCommand game) $ \command ->
        isJust $ run_ (apply command) game ^. see

verbose_runCommandErrors :: Game -> Command -> Property
verbose_runCommandErrors game command = whenFail (mapM_ putStrLn [show game, show command, show . fromRight $ run (apply command) game]) (isLeft $ run (apply command) game)
