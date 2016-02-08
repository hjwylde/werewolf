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
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.QuickCheck

prop_devourVoteCommandErrorsWhenGameIsOver :: Game -> Property
prop_devourVoteCommandErrorsWhenGameIsOver game =
    forAll (arbitraryDevourVoteCommand game') $ verbose_runCommandErrors game'
    where
        game' = game & stage .~ GameOver

prop_devourVoteCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_devourVoteCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_devourVoteCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_devourVoteCommandErrorsWhenTargetDoesNotExist game target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitraryWerewolf game) $ \caller -> do
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_devourVoteCommandErrorsWhenCallerIsDead :: Game -> Property
prop_devourVoteCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer game caller
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_devourVoteCommandErrorsWhenTargetIsDead :: Game -> Property
prop_devourVoteCommandErrorsWhenTargetIsDead game =
    forAll (arbitraryWerewolf game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer game target
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_devourVoteCommandErrorsWhenNotWerewolvesTurn :: Game -> Property
prop_devourVoteCommandErrorsWhenNotWerewolvesTurn game =
    not (isWerewolvesTurn game)
    ==> forAll (arbitraryDevourVoteCommand game) $ verbose_runCommandErrors game

prop_devourVoteCommandErrorsWhenCallerNotWerewolf :: Game -> Property
prop_devourVoteCommandErrorsWhenCallerNotWerewolf game =
    forAll (suchThat (arbitraryPlayer game) (not . isWerewolf)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = devourVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_devourVoteCommandErrorsWhenCallerHasVoted :: Game -> Property
prop_devourVoteCommandErrorsWhenCallerHasVoted game =
    forAll (arbitraryWerewolf game') $ \caller ->
    forAll (suchThat (arbitraryPlayer game') (not . isWerewolf)) $ \target -> do
        let command = devourVoteCommand (caller ^. name) (target ^. name)
        let game''  = run_ (apply command) game'

        verbose_runCommandErrors game'' command
    where
        game' = game & stage .~ WerewolvesTurn

prop_devourVoteCommandErrorsWhenTargetWerewolf :: Game -> Property
prop_devourVoteCommandErrorsWhenTargetWerewolf game =
    forAll (suchThat (arbitraryPlayer game) isWerewolf) $ \target ->
    forAll (arbitraryPlayer game) $ \caller ->
    verbose_runCommandErrors game (devourVoteCommand (caller ^. name) (target ^. name))

prop_devourVoteCommandUpdatesVotes :: Game -> Property
prop_devourVoteCommandUpdatesVotes game =
    forAll (arbitraryDevourVoteCommand game') $ \command -> do
        let game'' = run_ (apply command) game'

        Map.size (game'' ^. votes) == 1
    where
        game' = game & stage .~ WerewolvesTurn

prop_healCommandErrorsWhenGameIsOver :: Game -> Property
prop_healCommandErrorsWhenGameIsOver game = do
    let witch   = head . filterWitches $ game ^. players
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game' command
    where
        game' = game & stage .~ GameOver

prop_healCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_healCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> verbose_runCommandErrors game (healCommand (caller ^. name))

prop_healCommandErrorsWhenCallerIsDead :: Game -> Property
prop_healCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer game caller
        let command = healCommand (caller ^. name)

        verbose_runCommandErrors game' command

prop_healCommandErrorsWhenNoTargetIsDevoured :: Game -> Property
prop_healCommandErrorsWhenNoTargetIsDevoured game = do
    let witch   = head . filterWitches $ game ^. players
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_healCommandErrorsWhenNotWitchsTurn game = do
    let witch   = head . filterWitches $ game ^. players
    let command = healCommand $ witch ^. name

    not (isWitchsTurn game) ==> verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerHasHealed :: Gen Property
prop_healCommandErrorsWhenCallerHasHealed = do
    game <- arbitraryGameWithDevourEvent

    return $ forAll (arbitraryHealCommand game) $ \command -> do
        let game' = run_ (apply command) game

        verbose_runCommandErrors game' command

prop_healCommandErrorsWhenCallerNotWitch :: Game -> Property
prop_healCommandErrorsWhenCallerNotWitch game =
    forAll (suchThat (arbitraryPlayer game) (not . isWitch)) $ \caller -> do
        let command = healCommand (caller ^. name)

        verbose_runCommandErrors game command

prop_healCommandSetsHeal :: Gen Property
prop_healCommandSetsHeal = do
    game <- arbitraryGameWithDevourEvent

    return $ forAll (arbitraryHealCommand game) $ \command ->
        (run_ (apply command) game) ^. heal

prop_healCommandSetsHealUsed :: Gen Property
prop_healCommandSetsHealUsed = do
    game <- arbitraryGameWithDevourEvent

    return $ forAll (arbitraryHealCommand game) $ \command ->
        (run_ (apply command) game) ^. healUsed

prop_lynchVoteCommandErrorsWhenGameIsOver :: Game -> Property
prop_lynchVoteCommandErrorsWhenGameIsOver game =
    forAll (arbitraryLynchVoteCommand game') $ verbose_runCommandErrors game'
    where
        game' = game & stage .~ GameOver

prop_lynchVoteCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_lynchVoteCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_lynchVoteCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_lynchVoteCommandErrorsWhenTargetDoesNotExist game target =
    not (doesPlayerExist (target ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \caller -> do
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_lynchVoteCommandErrorsWhenCallerIsDead :: Game -> Property
prop_lynchVoteCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer game caller
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_lynchVoteCommandErrorsWhenTargetIsDead :: Game -> Property
prop_lynchVoteCommandErrorsWhenTargetIsDead game =
    forAll (arbitraryPlayer game) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer game target
        let command = lynchVoteCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_lynchVoteCommandErrorsWhenNotVillagesTurn :: Game -> Property
prop_lynchVoteCommandErrorsWhenNotVillagesTurn game =
    not (isVillagesTurn game)
    ==> forAll (arbitraryLynchVoteCommand game) $ verbose_runCommandErrors game

prop_lynchVoteCommandErrorsWhenCallerHasVoted :: Game -> Property
prop_lynchVoteCommandErrorsWhenCallerHasVoted game =
    forAll (arbitraryPlayer game') $ \caller ->
    forAll (arbitraryPlayer game') $ \target -> do
        let command = lynchVoteCommand (caller ^. name) (target ^. name)
        let game''  = run_ (apply command) game'

        verbose_runCommandErrors game'' command
    where
        game' = game & stage .~ VillagesTurn

prop_lynchVoteCommandUpdatesVotes :: Game -> Property
prop_lynchVoteCommandUpdatesVotes game =
    forAll (arbitraryLynchVoteCommand game') $ \command -> do
        let game'' = run_ (apply command) game'

        Map.size (game'' ^. votes) == 1
    where
        game' = game & stage .~ VillagesTurn

prop_passCommandErrorsWhenGameIsOver :: Game -> Property
prop_passCommandErrorsWhenGameIsOver game =
    forAll (arbitraryPassCommand game') $ verbose_runCommandErrors game'
    where
        game' = game & stage .~ GameOver

prop_passCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_passCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> verbose_runCommandErrors game (passCommand (caller ^. name))

prop_passCommandErrorsWhenCallerIsDead :: Game -> Property
prop_passCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer game caller
        let command = passCommand (caller ^. name)

        verbose_runCommandErrors game' command

prop_passCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_passCommandErrorsWhenNotWitchsTurn game =
    not (isWitchsTurn game)
    ==> forAll (arbitraryPassCommand game) $ verbose_runCommandErrors game

prop_passCommandUpdatesPasses :: Game -> Property
prop_passCommandUpdatesPasses game =
    forAll (arbitraryPassCommand game') $ \command -> do
        let game'' = run_ (apply command) game'

        length (game'' ^. passes) == 1
    where
        game' = game & stage .~ WitchsTurn

prop_poisonCommandErrorsWhenGameIsOver :: Game -> Property
prop_poisonCommandErrorsWhenGameIsOver game =
    forAll (arbitraryPoisonCommand game') $ verbose_runCommandErrors game'
    where
        game' = game & stage .~ GameOver

prop_poisonCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_poisonCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = poisonCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_poisonCommandErrorsWhenTargetDoesNotExist game target = do
    let witch   = head . filterWitches $ game ^. players
    let command = poisonCommand (witch ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) (game ^. players))
        ==> verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenCallerIsDead :: Game -> Property
prop_poisonCommandErrorsWhenCallerIsDead game = do
    let witch = head . filterWitches $ game ^. players

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer game witch
        let command = poisonCommand (witch ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_poisonCommandErrorsWhenTargetIsDead :: Game -> Property
prop_poisonCommandErrorsWhenTargetIsDead game = do
    let witch = head . filterWitches $ game ^. players

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer game target
        let command = poisonCommand (witch ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_poisonCommandErrorsWhenTargetIsDevoured :: Gen Property
prop_poisonCommandErrorsWhenTargetIsDevoured = do
    game                            <- arbitraryGameWithDevourEvent
    let (DevourEvent targetName)    = fromJust $ getDevourEvent game

    let witch   = head . filterWitches $ game ^. players
    let command = poisonCommand (witch ^. name) targetName

    return $ verbose_runCommandErrors game command

prop_poisonCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_poisonCommandErrorsWhenNotWitchsTurn game =
    not (isWitchsTurn game)
    ==> forAll (arbitraryPoisonCommand game) $ verbose_runCommandErrors game

prop_poisonCommandErrorsWhenCallerHasPoisoned :: Game -> Property
prop_poisonCommandErrorsWhenCallerHasPoisoned game = do
    forAll (arbitraryPoisonCommand game') $ \command -> do
        let game'' = run_ (apply command) game'

        verbose_runCommandErrors game'' command
    where
        game' = game & stage .~ WitchsTurn

prop_poisonCommandErrorsWhenCallerNotWitch :: Game -> Property
prop_poisonCommandErrorsWhenCallerNotWitch game =
    forAll (suchThat (arbitraryPlayer game) (not . isWitch)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = poisonCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_poisonCommandSetsPoison :: Game -> Property
prop_poisonCommandSetsPoison game =
    forAll (arbitraryPoisonCommand game') $ \command ->
    isJust (run_ (apply command) game' ^. poison)
    where
        game' = game & stage .~ WitchsTurn

prop_poisonCommandSetsPoisonUsed :: Game -> Property
prop_poisonCommandSetsPoisonUsed game =
    forAll (arbitraryPoisonCommand game') $ \command ->
    run_ (apply command) game' ^. poisonUsed
    where
        game' = game & stage .~ WitchsTurn

prop_protectCommandErrorsWhenGameIsOver :: Game -> Property
prop_protectCommandErrorsWhenGameIsOver game =
    forAll (arbitraryProtectCommand game') $ verbose_runCommandErrors game'
    where
        game' = game & stage .~ GameOver

prop_protectCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_protectCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = protectCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_protectCommandErrorsWhenTargetDoesNotExist game target = do
    let defender    = head . filterDefenders $ game ^. players
    let command     = protectCommand (defender ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) (game ^. players))
        ==> verbose_runCommandErrors game command

prop_protectCommandErrorsWhenCallerIsDead :: Game -> Property
prop_protectCommandErrorsWhenCallerIsDead game = do
    let defender    = head . filterDefenders $ game ^. players
    let game'       = killPlayer game defender

    forAll (arbitraryPlayer game') $ \target -> do
        let command = protectCommand (defender ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_protectCommandErrorsWhenTargetIsDead :: Game -> Property
prop_protectCommandErrorsWhenTargetIsDead game = do
    let defender = head . filterDefenders $ game ^. players

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer game target
        let command = protectCommand (defender ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_protectCommandErrorsWhenNotDefendersTurn :: Game -> Property
prop_protectCommandErrorsWhenNotDefendersTurn game =
    not (isDefendersTurn game)
    ==> forAll (arbitraryProtectCommand game) $ verbose_runCommandErrors game

prop_protectCommandErrorsWhenCallerNotDefender :: Game -> Property
prop_protectCommandErrorsWhenCallerNotDefender game =
    forAll (suchThat (arbitraryPlayer game) (not . isDefender)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = protectCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetIsCaller :: Game -> Property
prop_protectCommandErrorsWhenTargetIsCaller game = do
    let defender    = head . filterDefenders $ game ^. players
    let command     = protectCommand (defender ^. name) (defender ^. name)

    verbose_runCommandErrors game command

prop_protectCommandErrorsWhenTargetIsPriorProtect :: Gen Property
prop_protectCommandErrorsWhenTargetIsPriorProtect = do
    game        <- arbitraryGameWithProtect
    let game'   = game & protect .~ Nothing

    let defender    = head . filterDefenders $ game' ^. players
    let command     = protectCommand (defender ^. name) (fromJust $ game' ^. priorProtect)

    return $ verbose_runCommandErrors game' command

prop_protectCommandSetsPriorProtect :: Game -> Property
prop_protectCommandSetsPriorProtect game =
    forAll (arbitraryProtectCommand game') $ \command ->
    isJust $ run_ (apply command) game' ^. priorProtect
    where
        game' = game & stage .~ DefendersTurn

prop_protectCommandSetsProtect :: Game -> Property
prop_protectCommandSetsProtect game =
    forAll (arbitraryProtectCommand game') $ \command ->
    isJust $ run_ (apply command) game' ^. protect
    where
        game' = game & stage .~ DefendersTurn

prop_quitCommandErrorsWhenGameIsOver :: Game -> Property
prop_quitCommandErrorsWhenGameIsOver game =
    forAll (arbitraryQuitCommand game') $ verbose_runCommandErrors game'
    where
        game' = game & stage .~ GameOver

prop_quitCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_quitCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> verbose_runCommandErrors game (quitCommand $ caller ^. name)

prop_quitCommandErrorsWhenCallerIsDead :: Game -> Property
prop_quitCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer game caller
        let command = quitCommand $ caller ^. name

        verbose_runCommandErrors game' command

prop_quitCommandKillsPlayer :: Game -> Property
prop_quitCommandKillsPlayer game =
    not (isGameOver game)
    ==> forAll (arbitraryQuitCommand game) $ \command -> do
        let game' = run_ (apply command) game

        length (filterDead $ game' ^. players) == 1

prop_quitCommandClearsHealWhenCallerIsWitch :: Gen Bool
prop_quitCommandClearsHealWhenCallerIsWitch = do
    game        <- arbitraryGameWithHeal
    let witch   = head . filterWitches $ game ^. players
    let command = quitCommand (witch ^. name)

    return . not $ run_ (apply command) game ^. heal

prop_quitCommandClearsHealUsedWhenCallerIsWitch :: Gen Bool
prop_quitCommandClearsHealUsedWhenCallerIsWitch = do
    game        <- arbitraryGameWithHeal
    let witch   = head . filterWitches $ game ^. players
    let command = quitCommand (witch ^. name)

    return . not $ run_ (apply command) game ^. healUsed

prop_quitCommandClearsPoisonWhenCallerIsWitch :: Gen Bool
prop_quitCommandClearsPoisonWhenCallerIsWitch = do
    game        <- arbitraryGameWithPoison
    let witch   = head . filterWitches $ game ^. players
    let command = quitCommand (witch ^. name)

    return . isNothing $ run_ (apply command) game ^. poison

prop_quitCommandClearsPoisonUsedWhenCallerIsWitch :: Gen Bool
prop_quitCommandClearsPoisonUsedWhenCallerIsWitch = do
    game        <- arbitraryGameWithPoison
    let witch   = head . filterWitches $ game ^. players
    let command = quitCommand (witch ^. name)

    return . not $ run_ (apply command) game ^. poisonUsed

prop_quitCommandClearsPriorProtectWhenCallerIsDefender :: Gen Bool
prop_quitCommandClearsPriorProtectWhenCallerIsDefender = do
    game            <- arbitraryGameWithProtect
    let defender    = head . filterDefenders $ game ^. players
    let command     = quitCommand (defender ^. name)

    return . isNothing $ run_ (apply command) game ^. priorProtect

prop_quitCommandClearsProtectWhenCallerIsDefender :: Gen Bool
prop_quitCommandClearsProtectWhenCallerIsDefender = do
    game            <- arbitraryGameWithProtect
    let defender    = head . filterDefenders $ game ^. players
    let command     = quitCommand (defender ^. name)

    return . isNothing $ run_ (apply command) game ^. protect

prop_quitCommandClearsPlayersDevourVote :: Gen Property
prop_quitCommandClearsPlayersDevourVote = do
    game <- arbitraryGameWithDevourVotes

    return $ forAll (arbitraryWerewolf game) $ \caller -> do
        let command = quitCommand (caller ^. name)

        isNothing $ run_ (apply command) game ^. votes . at (caller ^. name)

prop_quitCommandClearsPlayersLynchVote :: Gen Property
prop_quitCommandClearsPlayersLynchVote = do
    game <- arbitraryGameWithLynchVotes

    return $ forAll (arbitraryPlayer game) $ \caller -> do
        let command = quitCommand (caller ^. name)

        isNothing $ run_ (apply command) game ^. votes . at (caller ^. name)

prop_seeCommandErrorsWhenGameIsOver :: Game -> Property
prop_seeCommandErrorsWhenGameIsOver game =
    forAll (arbitrarySeeCommand game') $ verbose_runCommandErrors game'
    where
        game' = game & stage .~ GameOver

prop_seeCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_seeCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) (game ^. players))
    ==> forAll (arbitraryPlayer game) $ \target -> do
        let command = seeCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_seeCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_seeCommandErrorsWhenTargetDoesNotExist game target = do
    let seer    = head . filterSeers $ game ^. players
    let command = seeCommand (seer ^. name) (target ^. name)

    not (doesPlayerExist (target ^. name) (game ^. players))
        ==> verbose_runCommandErrors game command

prop_seeCommandErrorsWhenCallerIsDead :: Game -> Property
prop_seeCommandErrorsWhenCallerIsDead game = do
    let seer    = head . filterSeers $ game ^. players
    let game'   = killPlayer game seer

    forAll (arbitraryPlayer game') $ \target -> do
        let command = seeCommand (seer ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_seeCommandErrorsWhenTargetIsDead :: Game -> Property
prop_seeCommandErrorsWhenTargetIsDead game = do
    let seer = head . filterSeers $ game ^. players

    forAll (arbitraryPlayer game) $ \target -> do
        let game'   = killPlayer game target
        let command = seeCommand (seer ^. name) (target ^. name)

        verbose_runCommandErrors game' command

prop_seeCommandErrorsWhenNotSeersTurn :: Game -> Property
prop_seeCommandErrorsWhenNotSeersTurn game =
    not (isSeersTurn game)
    ==> forAll (arbitrarySeeCommand game) $ verbose_runCommandErrors game

prop_seeCommandErrorsWhenCallerNotSeer :: Game -> Property
prop_seeCommandErrorsWhenCallerNotSeer game =
    forAll (suchThat (arbitraryPlayer game) (not . isSeer)) $ \caller ->
    forAll (arbitraryPlayer game) $ \target -> do
        let command = seeCommand (caller ^. name) (target ^. name)

        verbose_runCommandErrors game command

prop_seeCommandSetsSee :: Game -> Property
prop_seeCommandSetsSee game =
    forAll (arbitrarySeeCommand game') $ \command ->
    isJust $ run_ (apply command) game' ^. see
    where
        game' = game & stage .~ SeersTurn

verbose_runCommandErrors :: Game -> Command -> Property
verbose_runCommandErrors game command = whenFail (mapM_ putStrLn data_) (isLeft result)
    where
        result  = run (apply command) game
        data_   = [show game, show $ fromRight result]
