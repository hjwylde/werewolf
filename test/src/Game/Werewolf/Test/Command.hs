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

    -- * passCommand
    prop_passCommandErrorsWhenGameIsOver, prop_passCommandErrorsWhenCallerDoesNotExist,
    prop_passCommandErrorsWhenCallerIsDead, prop_passCommandErrorsWhenNotWitchsTurn,
    prop_passCommandUpdatesPasses,

    -- * poisonCommand
    prop_poisonCommandErrorsWhenGameIsOver, prop_poisonCommandErrorsWhenCallerDoesNotExist,
    prop_poisonCommandErrorsWhenTargetDoesNotExist, prop_poisonCommandErrorsWhenCallerIsDead,
    prop_poisonCommandErrorsWhenTargetIsDead, prop_poisonCommandErrorsWhenTargetIsDevoured,
    prop_poisonCommandErrorsWhenNotWitchsTurn, prop_poisonCommandErrorsWhenCallerHasPoisoned,
    prop_poisonCommandErrorsWhenCallerNotWitch, prop_poisonCommandSetsPoison,

    -- * quitCommand
    prop_quitCommandErrorsWhenGameIsOver, prop_quitCommandErrorsWhenCallerDoesNotExist,
    prop_quitCommandErrorsWhenCallerIsDead, prop_quitCommandKillsPlayer,
    prop_quitCommandClearsPlayersDevourVote, prop_quitCommandClearsPlayersLynchVote,
    prop_quitCommandClearsPlayersPoison,

    -- * seeCommand
    prop_seeCommandErrorsWhenGameIsOver, prop_seeCommandErrorsWhenCallerDoesNotExist,
    prop_seeCommandErrorsWhenTargetDoesNotExist, prop_seeCommandErrorsWhenCallerIsDead,
    prop_seeCommandErrorsWhenTargetIsDead, prop_seeCommandErrorsWhenNotSeersTurn,
    prop_seeCommandErrorsWhenCallerNotSeer, prop_seeCommandSetsSee,
) where

import Control.Lens hiding (elements)

import           Data.Either.Extra
import qualified Data.Map          as Map
import           Data.Maybe

import Game.Werewolf.Command
import Game.Werewolf.Engine         (checkStage)
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
    forAll (suchThat (arbitraryPlayer game) (not . isWerewolf)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors game (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandErrorsWhenCallerHasVoted :: Game -> Property
prop_devourVoteCommandErrorsWhenCallerHasVoted game =
    forAll (arbitraryWerewolf game') $ \caller ->
    forAll (suchThat (arbitraryPlayer game') (not . isWerewolf)) $ \target ->
    let command = devourVoteCommand (caller ^. name) (target ^. name)
    in verbose_runCommandErrors (run_ (apply command) game') command
    where
        game' = game { _stage = WerewolvesTurn }

prop_devourVoteCommandErrorsWhenTargetWerewolf :: Game -> Property
prop_devourVoteCommandErrorsWhenTargetWerewolf game =
    forAll (suchThat (arbitraryPlayer game) isWerewolf) $ \target ->
    forAll (arbitraryPlayer game) $ \caller ->
    verbose_runCommandErrors game (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandUpdatesVotes :: Game -> Property
prop_devourVoteCommandUpdatesVotes game =
    forAll (arbitraryDevourVoteCommand game') $ \command ->
    Map.size (run_ (apply command) game' ^. votes) == 1
    where
        game' = game { _stage = WerewolvesTurn }

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
    forAll (arbitraryPlayer game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    let command = lynchVoteCommand (caller ^. name) (target ^. name)
    in verbose_runCommandErrors (run_ (apply command) game') command
    where
        game' = game { _stage = VillagesTurn }

prop_lynchVoteCommandUpdatesVotes :: Game -> Property
prop_lynchVoteCommandUpdatesVotes game =
    forAll (arbitraryLynchVoteCommand game') $ \command ->
    Map.size (run_ (apply command) game' ^. votes) == 1
    where
        game' = game { _stage = VillagesTurn }

prop_passCommandErrorsWhenGameIsOver :: Game -> Property
prop_passCommandErrorsWhenGameIsOver game =
    isGameOver game
    ==> forAll (arbitraryPassCommand game) $ verbose_runCommandErrors game

prop_passCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_passCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> verbose_runCommandErrors game (passCommand (caller ^. name))

prop_passCommandErrorsWhenCallerIsDead :: Game -> Property
prop_passCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller ->
    verbose_runCommandErrors (killPlayer game caller) (passCommand (caller ^. name))

prop_passCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_passCommandErrorsWhenNotWitchsTurn game =
    not (isWitchsTurn game)
    ==> forAll (arbitraryPassCommand game) $ verbose_runCommandErrors game

prop_passCommandUpdatesPasses :: Game -> Property
prop_passCommandUpdatesPasses game =
    forAll (arbitraryPassCommand game') $ \command ->
    length (run_ (apply command) game' ^. passes) == 1
    where
        game' = game { _stage = WitchsTurn }

prop_poisonCommandErrorsWhenGameIsOver :: Game -> Property
prop_poisonCommandErrorsWhenGameIsOver game =
    isGameOver game
    ==> forAll (arbitraryPoisonCommand game) $ verbose_runCommandErrors game

prop_poisonCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_poisonCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (poisonCommand (caller ^. name) (target ^. name))

prop_poisonCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_poisonCommandErrorsWhenTargetDoesNotExist game target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \caller ->
        verbose_runCommandErrors game (poisonCommand (caller ^. name) (target ^. name))

prop_poisonCommandErrorsWhenCallerIsDead :: Game -> Property
prop_poisonCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game caller) (poisonCommand (caller ^. name) (target ^. name))

prop_poisonCommandErrorsWhenTargetIsDead :: Game -> Property
prop_poisonCommandErrorsWhenTargetIsDead game =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors (killPlayer game target) (poisonCommand (caller ^. name) (target ^. name))

prop_poisonCommandErrorsWhenTargetIsDevoured :: Game -> Property
prop_poisonCommandErrorsWhenTargetIsDevoured game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') == 1
    ==> forAll (arbitraryWitch game'') $ \caller ->
        let game''' = run_ checkStage game''
            votee   = head (getVoteResult game'')
        in verbose_runCommandErrors game''' (poisonCommand (caller ^. name) (votee ^. name))
    where
        game'   = game { _stage = WerewolvesTurn }
        n       = length . filterWerewolves $ game' ^. players

prop_poisonCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_poisonCommandErrorsWhenNotWitchsTurn game =
    not (isWitchsTurn game)
    ==> forAll (arbitraryPoisonCommand game) $ verbose_runCommandErrors game

prop_poisonCommandErrorsWhenCallerHasPoisoned :: Game -> Property
prop_poisonCommandErrorsWhenCallerHasPoisoned game =
    forAll (arbitraryWitch game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    let command = poisonCommand (caller ^. name) (target ^. name)
    in verbose_runCommandErrors (run_ (apply command) game') command
    where
        game' = game { _stage = WitchsTurn }

prop_poisonCommandErrorsWhenCallerNotWitch :: Game -> Property
prop_poisonCommandErrorsWhenCallerNotWitch game =
    forAll (suchThat (arbitraryPlayer game) (not . isWitch)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
        verbose_runCommandErrors game (poisonCommand (caller ^. name) (target ^. name))

prop_poisonCommandSetsPoison :: Game -> Property
prop_poisonCommandSetsPoison game =
    forAll (arbitraryPoisonCommand game') $ \command ->
    isJust (run_ (apply command) game' ^. poison)
    where
        game' = game { _stage = WitchsTurn }

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
    forAll (suchThat (arbitraryPlayer game') (not . isWerewolf)) $ \target ->
    let game'' = run_ (apply $ devourVoteCommand (caller ^. name) (target ^. name)) game'
    in Map.null $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. votes
    where
        game' = game { _stage = WerewolvesTurn }

prop_quitCommandClearsPlayersLynchVote :: Game -> Property
prop_quitCommandClearsPlayersLynchVote game =
    forAll (arbitraryPlayer game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    let game'' = run_ (apply $ lynchVoteCommand (caller ^. name) (target ^. name)) game'
        in Map.null $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. votes
    where
        game' = game { _stage = VillagesTurn }

prop_quitCommandClearsPlayersPoison :: Game -> Property
prop_quitCommandClearsPlayersPoison game =
    forAll (arbitraryWitch game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target ->
    let game'' = run_ (apply $ poisonCommand (caller ^. name) (target ^. name)) game'
    in isNothing $ run_ (apply $ quitCommand (caller ^. name)) game'' ^. poison
    where
        game' = game { _stage = WitchsTurn }

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
    forAll (suchThat (arbitraryPlayer game) (not . isSeer)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target ->
    verbose_runCommandErrors game (seeCommand (caller ^. name) (target ^. name))

prop_seeCommandSetsSee :: Game -> Property
prop_seeCommandSetsSee game =
    forAll (arbitrarySeeCommand game') $ \command ->
    isJust $ run_ (apply command) game' ^. see
    where
        game' = game { _stage = SeersTurn }

verbose_runCommandErrors :: Game -> Command -> Property
verbose_runCommandErrors game command = whenFail (mapM_ putStrLn [show game, show . fromRight $ run (apply command) game]) (isLeft $ run (apply command) game)
