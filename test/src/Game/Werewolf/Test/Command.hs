{-|
Module      : Game.Werewolf.Test.Command
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Game.Werewolf.Test.Command (
    -- * chooseCommand
    prop_chooseCommandErrorsWhenGameIsOver, prop_chooseCommandErrorsWhenCallerDoesNotExist,
    prop_chooseCommandErrorsWhenCallerIsDead, prop_chooseCommandErrorsWhenNotWolfHoundsTurn,
    prop_chooseCommandErrorsWhenCallerNotWolfHound, prop_chooseCommandSetsCallersRole,

    -- * devourVoteCommand
    prop_devourVoteCommandErrorsWhenGameIsOver, prop_devourVoteCommandErrorsWhenCallerDoesNotExist,
    prop_devourVoteCommandErrorsWhenTargetDoesNotExist,
    prop_devourVoteCommandErrorsWhenCallerIsDead, prop_devourVoteCommandErrorsWhenTargetIsDead,
    prop_devourVoteCommandErrorsWhenNotWerewolvesTurn,
    prop_devourVoteCommandErrorsWhenCallerNotAlignedWithWerewolves,
    prop_devourVoteCommandErrorsWhenCallerHasVoted,
    prop_devourVoteCommandErrorsWhenTargetAlignedWithWerewolves, prop_devourVoteCommandUpdatesVotes,

    -- * healCommand
    prop_healCommandErrorsWhenGameIsOver, prop_healCommandErrorsWhenCallerDoesNotExist,
    prop_healCommandErrorsWhenCallerIsDead, prop_healCommandErrorsWhenNoTargetIsDevoured,
    prop_healCommandErrorsWhenNotWitchsTurn, prop_healCommandErrorsWhenCallerHasHealed,
    prop_healCommandErrorsWhenCallerNotWitch, prop_healCommandSetsHeal,
    prop_healCommandSetsHealUsed,

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
    prop_poisonCommandSetsPoisonUsed,

    -- * protectCommand
    prop_protectCommandErrorsWhenGameIsOver, prop_protectCommandErrorsWhenCallerDoesNotExist,
    prop_protectCommandErrorsWhenTargetDoesNotExist, prop_protectCommandErrorsWhenCallerIsDead,
    prop_protectCommandErrorsWhenTargetIsDead, prop_protectCommandErrorsWhenNotDefendersTurn,
    prop_protectCommandErrorsWhenCallerNotDefender, prop_protectCommandErrorsWhenTargetIsCaller,
    prop_protectCommandErrorsWhenTargetIsPriorProtect, prop_protectCommandSetsPriorProtect,
    prop_protectCommandSetsProtect,

    -- * quitCommand
    prop_quitCommandErrorsWhenGameIsOver, prop_quitCommandErrorsWhenCallerDoesNotExist,
    prop_quitCommandErrorsWhenCallerIsDead, prop_quitCommandKillsPlayer,
    prop_quitCommandClearsHealWhenCallerIsWitch, prop_quitCommandClearsHealUsedWhenCallerIsWitch,
    prop_quitCommandClearsPoisonWhenCallerIsWitch,
    prop_quitCommandClearsPoisonUsedWhenCallerIsWitch,
    prop_quitCommandClearsPriorProtectWhenCallerIsDefender,
    prop_quitCommandClearsProtectWhenCallerIsDefender, prop_quitCommandClearsPlayersDevourVote,
    prop_quitCommandClearsPlayersLynchVote,

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
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Role           hiding (name)
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.QuickCheck

prop_chooseCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_chooseCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_chooseCommandErrorsWhenCallerDoesNotExist :: GameAtWolfHoundsTurn -> Player -> Allegiance -> Property
prop_chooseCommandErrorsWhenCallerDoesNotExist (GameAtWolfHoundsTurn game) caller allegiance = do
    let command = chooseCommand (caller ^. name) allegiance

    not (doesPlayerExist (caller ^. name) (game ^. players))
        ==> verbose_runCommandErrors game command

prop_chooseCommandErrorsWhenCallerIsDead :: GameAtWolfHoundsTurn -> Allegiance -> Property
prop_chooseCommandErrorsWhenCallerIsDead (GameAtWolfHoundsTurn game) allegiance = do
    let wolfHound   = findByRole_ wolfHoundRole (game ^. players)
    let game'       = killPlayer (wolfHound ^. name) game
    let command     = chooseCommand (wolfHound ^. name) allegiance

    verbose_runCommandErrors game' command

prop_chooseCommandErrorsWhenNotWolfHoundsTurn :: Game -> Property
prop_chooseCommandErrorsWhenNotWolfHoundsTurn game =
    not (isWolfHoundsTurn game)
    ==> forAll (arbitraryChooseCommand game) $ verbose_runCommandErrors game . getBlind

prop_chooseCommandErrorsWhenCallerNotWolfHound :: GameAtWolfHoundsTurn -> Allegiance -> Property
prop_chooseCommandErrorsWhenCallerNotWolfHound (GameAtWolfHoundsTurn game) allegiance =
    forAll (suchThat (arbitraryPlayer game) (not . isWolfHound)) $ \caller -> do
        let command = chooseCommand (caller ^. name) allegiance

        verbose_runCommandErrors game command

prop_chooseCommandSetsCallersRole :: GameAtWolfHoundsTurn -> Allegiance -> Property
prop_chooseCommandSetsCallersRole (GameAtWolfHoundsTurn game) allegiance' = do
    let wolfHound   = findByRole_ wolfHoundRole (game ^. players)
    let command     = chooseCommand (wolfHound ^. name) allegiance'
    let game'       = run_ (apply command) game

    findByRole_ wolfHoundRole (game' ^. players) ^. role === role'
    where
        role' = case allegiance' of
            Villagers   -> simpleVillagerRole
            Werewolves  -> simpleWerewolfRole

prop_devourVoteCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_devourVoteCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryDevourVoteCommand game) $ verbose_runCommandErrors game . getBlind

prop_devourVoteCommandErrorsWhenCallerDoesNotExist :: GameAtWerewolvesTurn -> Player -> Property
prop_devourVoteCommandErrorsWhenCallerDoesNotExist (GameAtWerewolvesTurn game) caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_devourVoteCommandErrorsWhenTargetDoesNotExist :: GameAtWerewolvesTurn -> Player -> Property
prop_devourVoteCommandErrorsWhenTargetDoesNotExist (GameAtWerewolvesTurn game) target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitraryPlayerAlignedWithWerewolves game) $ \caller -> do
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_devourVoteCommandErrorsWhenCallerIsDead :: GameAtWerewolvesTurn -> Property
prop_devourVoteCommandErrorsWhenCallerIsDead (GameAtWerewolvesTurn game) =
    forAll (arbitraryPlayerAlignedWithWerewolves game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (caller ^. name) game
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_devourVoteCommandErrorsWhenTargetIsDead :: GameAtWerewolvesTurn -> Property
prop_devourVoteCommandErrorsWhenTargetIsDead (GameAtWerewolvesTurn game) =
    forAll (arbitraryPlayerAlignedWithWerewolves game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_devourVoteCommandErrorsWhenNotWerewolvesTurn :: Game -> Property
prop_devourVoteCommandErrorsWhenNotWerewolvesTurn game =
    not (isWerewolvesTurn game)
    ==> forAll (arbitraryDevourVoteCommand game) $ verbose_runCommandErrors game . getBlind

prop_devourVoteCommandErrorsWhenCallerNotAlignedWithWerewolves :: GameAtWerewolvesTurn -> Property
prop_devourVoteCommandErrorsWhenCallerNotAlignedWithWerewolves (GameAtWerewolvesTurn game) =
    forAll (suchThat (arbitraryPlayer game) (not . isAlignedWithWerewolves)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_devourVoteCommandErrorsWhenCallerHasVoted :: GameWithDevourVotes -> Property
prop_devourVoteCommandErrorsWhenCallerHasVoted (GameWithDevourVotes game) =
    forAll (arbitraryPlayerAlignedWithWerewolves game) $ \caller ->
    forAll (suchThat (arbitraryPlayer game) (not . isAlignedWithWerewolves)) $ \target -> do
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_devourVoteCommandErrorsWhenTargetAlignedWithWerewolves :: GameAtWerewolvesTurn -> Property
prop_devourVoteCommandErrorsWhenTargetAlignedWithWerewolves (GameAtWerewolvesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayerAlignedWithWerewolves game) $ \target ->
    verbose_runCommandErrors game (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandUpdatesVotes :: GameAtWerewolvesTurn -> Property
prop_devourVoteCommandUpdatesVotes (GameAtWerewolvesTurn game) =
    forAll (arbitraryDevourVoteCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        Map.size (game' ^. votes) == 1

prop_healCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_healCommandErrorsWhenGameIsOver (GameAtGameOver game) = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerDoesNotExist :: GameWithDevourEvent -> Player -> Property
prop_healCommandErrorsWhenCallerDoesNotExist (GameWithDevourEvent game) caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> verbose_runCommandErrors game (healCommand (caller ^. name))

prop_healCommandErrorsWhenCallerIsDead :: GameWithDevourEvent -> Property
prop_healCommandErrorsWhenCallerIsDead (GameWithDevourEvent game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = healCommand (caller ^. name)

        verbose_runCommandErrors game' command

prop_healCommandErrorsWhenNoTargetIsDevoured :: GameAtWitchsTurn -> Property
prop_healCommandErrorsWhenNoTargetIsDevoured (GameAtWitchsTurn game) = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_healCommandErrorsWhenNotWitchsTurn game = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = healCommand $ witch ^. name

    not (isWitchsTurn game) ==> verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerHasHealed :: GameWithHeal -> Property
prop_healCommandErrorsWhenCallerHasHealed (GameWithHeal game) = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerNotWitch :: GameWithDevourEvent -> Property
prop_healCommandErrorsWhenCallerNotWitch (GameWithDevourEvent game) =
    forAll (suchThat (arbitraryPlayer game) (not . isWitch)) $ \caller -> do
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

prop_lynchVoteCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_lynchVoteCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryLynchVoteCommand game) $ verbose_runCommandErrors game . getBlind

prop_lynchVoteCommandErrorsWhenCallerDoesNotExist :: GameAtVillagesTurn -> Player -> Property
prop_lynchVoteCommandErrorsWhenCallerDoesNotExist (GameAtVillagesTurn game) caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_lynchVoteCommandErrorsWhenTargetDoesNotExist :: GameAtVillagesTurn -> Player -> Property
prop_lynchVoteCommandErrorsWhenTargetDoesNotExist (GameAtVillagesTurn game) target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \caller -> do
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_lynchVoteCommandErrorsWhenCallerIsDead :: GameAtVillagesTurn -> Property
prop_lynchVoteCommandErrorsWhenCallerIsDead (GameAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (caller ^. name) game
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_lynchVoteCommandErrorsWhenTargetIsDead :: GameAtVillagesTurn -> Property
prop_lynchVoteCommandErrorsWhenTargetIsDead (GameAtVillagesTurn game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_lynchVoteCommandErrorsWhenNotVillagesTurn :: Game -> Property
prop_lynchVoteCommandErrorsWhenNotVillagesTurn game =
    not (isVillagesTurn game)
    ==> forAll (arbitraryLynchVoteCommand game) $ verbose_runCommandErrors game . getBlind

prop_lynchVoteCommandErrorsWhenCallerHasVoted :: GameWithLynchVotes -> Property
prop_lynchVoteCommandErrorsWhenCallerHasVoted (GameWithLynchVotes game) =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_lynchVoteCommandUpdatesVotes :: GameAtVillagesTurn -> Property
prop_lynchVoteCommandUpdatesVotes (GameAtVillagesTurn game) =
    forAll (arbitraryLynchVoteCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        Map.size (game' ^. votes) == 1

prop_passCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_passCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_passCommandErrorsWhenCallerDoesNotExist :: GameAtWitchsTurn -> Player -> Property
prop_passCommandErrorsWhenCallerDoesNotExist (GameAtWitchsTurn game) caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> verbose_runCommandErrors game (passCommand (caller ^. name))

prop_passCommandErrorsWhenCallerIsDead :: GameAtWitchsTurn -> Property
prop_passCommandErrorsWhenCallerIsDead (GameAtWitchsTurn game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = passCommand (caller ^. name)

        verbose_runCommandErrors game' command

prop_passCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_passCommandErrorsWhenNotWitchsTurn game =
    not (isWitchsTurn game)
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
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = poisonCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenTargetDoesNotExist :: GameAtWitchsTurn -> Player -> Property
prop_poisonCommandErrorsWhenTargetDoesNotExist (GameAtWitchsTurn game) target = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = poisonCommand (witch ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) (game ^. players))
        ==> verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenCallerIsDead :: GameAtWitchsTurn -> Property
prop_poisonCommandErrorsWhenCallerIsDead (GameAtWitchsTurn game) = do
    let witch = findByRole_ witchRole (game ^. players)

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (witch ^. name) game
        let command = poisonCommand (witch ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_poisonCommandErrorsWhenTargetIsDead :: GameAtWitchsTurn -> Property
prop_poisonCommandErrorsWhenTargetIsDead (GameAtWitchsTurn game) = do
    let witch = findByRole_ witchRole (game ^. players)

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = poisonCommand (witch ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_poisonCommandErrorsWhenTargetIsDevoured :: GameWithDevourEvent -> Property
prop_poisonCommandErrorsWhenTargetIsDevoured (GameWithDevourEvent game) = do
    let (DevourEvent targetName) = fromJust $ getDevourEvent game

    let witch   = findByRole_ witchRole (game ^. players)
    let command = poisonCommand (witch ^. name) targetName

    verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_poisonCommandErrorsWhenNotWitchsTurn game =
    not (isWitchsTurn game)
    ==> forAll (arbitraryPoisonCommand game) $ verbose_runCommandErrors game . getBlind

prop_poisonCommandErrorsWhenCallerHasPoisoned :: GameWithPoison -> Property
prop_poisonCommandErrorsWhenCallerHasPoisoned (GameWithPoison game) = do
    let witch = findByRole_ witchRole (game ^. players)

    forAll (arbitraryPlayer game) $ \target -> do
        let command = poisonCommand (witch ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenCallerNotWitch :: GameAtWitchsTurn -> Property
prop_poisonCommandErrorsWhenCallerNotWitch (GameAtWitchsTurn game) =
    forAll (suchThat (arbitraryPlayer game) (not . isWitch)) $ \caller ->
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
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = protectCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetDoesNotExist :: GameAtDefendersTurn -> Player -> Property
prop_protectCommandErrorsWhenTargetDoesNotExist (GameAtDefendersTurn game) target = do
    let defender    = findByRole_ defenderRole (game ^. players)
    let command     = protectCommand (defender ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) (game ^. players))
        ==> verbose_runCommandErrors game command

prop_protectCommandErrorsWhenCallerIsDead :: GameAtDefendersTurn -> Property
prop_protectCommandErrorsWhenCallerIsDead (GameAtDefendersTurn game) = do
    let defender    = findByRole_ defenderRole (game ^. players)
    let game'       = killPlayer (defender ^. name) game

    forAll (arbitraryPlayer game') $ \target -> do
        let command = protectCommand (defender ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_protectCommandErrorsWhenTargetIsDead :: GameAtDefendersTurn -> Property
prop_protectCommandErrorsWhenTargetIsDead (GameAtDefendersTurn game) = do
    let defender = findByRole_ defenderRole (game ^. players)

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = protectCommand (defender ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_protectCommandErrorsWhenNotDefendersTurn :: Game -> Property
prop_protectCommandErrorsWhenNotDefendersTurn game =
    not (isDefendersTurn game)
    ==> forAll (arbitraryProtectCommand game) $ verbose_runCommandErrors game . getBlind

prop_protectCommandErrorsWhenCallerNotDefender :: GameAtDefendersTurn -> Property
prop_protectCommandErrorsWhenCallerNotDefender (GameAtDefendersTurn game) =
    forAll (suchThat (arbitraryPlayer game) (not . isDefender)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = protectCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetIsCaller :: GameAtDefendersTurn -> Property
prop_protectCommandErrorsWhenTargetIsCaller (GameAtDefendersTurn game) = do
    let defender    = findByRole_ defenderRole (game ^. players)
    let command     = protectCommand (defender ^. name) (defender ^. name)

    verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetIsPriorProtect :: GameWithProtect -> Property
prop_protectCommandErrorsWhenTargetIsPriorProtect (GameWithProtect game) = do
    let game' = game & protect .~ Nothing

    let defender    = findByRole_ defenderRole (game' ^. players)
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
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> verbose_runCommandErrors game (quitCommand $ caller ^. name)

prop_quitCommandErrorsWhenCallerIsDead :: Game -> Property
prop_quitCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = quitCommand $ caller ^. name

        verbose_runCommandErrors game' command

prop_quitCommandKillsPlayer :: Game -> Property
prop_quitCommandKillsPlayer game =
    not (isGameOver game)
    ==> forAll (arbitraryQuitCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        length (filterDead $ game' ^. players) == 1

prop_quitCommandClearsHealWhenCallerIsWitch :: GameWithHeal -> Bool
prop_quitCommandClearsHealWhenCallerIsWitch (GameWithHeal game) = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = quitCommand (witch ^. name)

    not $ run_ (apply command) game ^. heal

prop_quitCommandClearsHealUsedWhenCallerIsWitch :: GameWithHeal -> Bool
prop_quitCommandClearsHealUsedWhenCallerIsWitch (GameWithHeal game) = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = quitCommand (witch ^. name)

    not $ run_ (apply command) game ^. healUsed

prop_quitCommandClearsPoisonWhenCallerIsWitch :: GameWithPoison -> Bool
prop_quitCommandClearsPoisonWhenCallerIsWitch (GameWithPoison game) = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = quitCommand (witch ^. name)

    isNothing $ run_ (apply command) game ^. poison

prop_quitCommandClearsPoisonUsedWhenCallerIsWitch :: GameWithPoison -> Bool
prop_quitCommandClearsPoisonUsedWhenCallerIsWitch (GameWithPoison game) = do
    let witch   = findByRole_ witchRole (game ^. players)
    let command = quitCommand (witch ^. name)

    not $ run_ (apply command) game ^. poisonUsed

prop_quitCommandClearsPriorProtectWhenCallerIsDefender :: GameWithProtect -> Bool
prop_quitCommandClearsPriorProtectWhenCallerIsDefender (GameWithProtect game) = do
    let defender    = findByRole_ defenderRole (game ^. players)
    let command     = quitCommand (defender ^. name)

    isNothing $ run_ (apply command) game ^. priorProtect

prop_quitCommandClearsProtectWhenCallerIsDefender :: GameWithProtect -> Bool
prop_quitCommandClearsProtectWhenCallerIsDefender (GameWithProtect game) = do
    let defender    = findByRole_ defenderRole (game ^. players)
    let command     = quitCommand (defender ^. name)

    isNothing $ run_ (apply command) game ^. protect

prop_quitCommandClearsPlayersDevourVote :: GameWithDevourVotes -> Property
prop_quitCommandClearsPlayersDevourVote (GameWithDevourVotes game) =
    forAll (arbitraryPlayerAlignedWithWerewolves game) $ \caller -> do
        let command = quitCommand (caller ^. name)

        isNothing $ run_ (apply command) game ^. votes . at (caller ^. name)

prop_quitCommandClearsPlayersLynchVote :: GameWithLynchVotes -> Property
prop_quitCommandClearsPlayersLynchVote (GameWithLynchVotes game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let command = quitCommand (caller ^. name)

        isNothing $ run_ (apply command) game ^. votes . at (caller ^. name)

prop_seeCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_seeCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitrarySeeCommand game) $ verbose_runCommandErrors game . getBlind

prop_seeCommandErrorsWhenCallerDoesNotExist :: GameAtSeersTurn -> Player -> Property
prop_seeCommandErrorsWhenCallerDoesNotExist (GameAtSeersTurn game) caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = seeCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_seeCommandErrorsWhenTargetDoesNotExist :: GameAtSeersTurn -> Player -> Property
prop_seeCommandErrorsWhenTargetDoesNotExist (GameAtSeersTurn game) target = do
    let seer    = findByRole_ seerRole (game ^. players)
    let command = seeCommand (seer ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) (game ^. players))
        ==> verbose_runCommandErrors game command

prop_seeCommandErrorsWhenCallerIsDead :: GameAtSeersTurn -> Property
prop_seeCommandErrorsWhenCallerIsDead (GameAtSeersTurn game) = do
    let seer    = findByRole_ seerRole (game ^. players)
    let game'   = killPlayer (seer ^. name) game

    forAll (arbitraryPlayer game') $ \target -> do
        let command = seeCommand (seer ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_seeCommandErrorsWhenTargetIsDead :: GameAtSeersTurn -> Property
prop_seeCommandErrorsWhenTargetIsDead (GameAtSeersTurn game) = do
    let seer = findByRole_ seerRole (game ^. players)

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer (target ^. name) game
        let command = seeCommand (seer ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_seeCommandErrorsWhenNotSeersTurn :: Game -> Property
prop_seeCommandErrorsWhenNotSeersTurn game =
    not (isSeersTurn game)
    ==> forAll (arbitrarySeeCommand game) $ verbose_runCommandErrors game . getBlind

prop_seeCommandErrorsWhenCallerNotSeer :: GameAtSeersTurn -> Property
prop_seeCommandErrorsWhenCallerNotSeer (GameAtSeersTurn game) =
    forAll (suchThat (arbitraryPlayer game) (not . isSeer)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = seeCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_seeCommandSetsSee :: GameAtSeersTurn -> Property
prop_seeCommandSetsSee (GameAtSeersTurn game) =
    forAll (arbitrarySeeCommand game) $ \(Blind command) ->
    isJust $ run_ (apply command) game ^. see

verbose_runCommandErrors :: Game -> Command -> Property
verbose_runCommandErrors game command = whenFail (mapM_ putStrLn data_) (isLeft result)
    where
        result  = run (apply command) game
        data_   = [show game, show $ fromRight result]
