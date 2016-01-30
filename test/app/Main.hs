{-|
Module      : Main
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Main (
    main
) where

import Game.Werewolf.Test.Arbitrary ()
import Game.Werewolf.Test.Command
import Game.Werewolf.Test.Engine
import Game.Werewolf.Test.Game
import Game.Werewolf.Test.Player

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = return $ testGroup "Tests" (concat [allCommandTests, allEngineTests, allGameTests, allPlayerTests])

allCommandTests :: [TestTree]
allCommandTests = [
    testProperty "PROP: devour vote command errors when game is over" prop_devourVoteCommandErrorsWhenGameIsOver,
    testProperty "PROP: devour vote command errors when caller does not exist" prop_devourVoteCommandErrorsWhenCallerDoesNotExist,
    testProperty "PROP: devour vote command errors when target does not exist" prop_devourVoteCommandErrorsWhenTargetDoesNotExist,
    testProperty "PROP: devour vote command errors when caller is dead" prop_devourVoteCommandErrorsWhenCallerIsDead,
    testProperty "PROP: devour vote command errors when target is dead" prop_devourVoteCommandErrorsWhenTargetIsDead,
    testProperty "PROP: devour vote command errors when not werewolves turn" prop_devourVoteCommandErrorsWhenNotWerewolvesTurn,
    testProperty "PROP: devour vote command errors when caller not werewolf" prop_devourVoteCommandErrorsWhenCallerNotWerewolf,
    testProperty "PROP: devour vote command errors when caller has voted" prop_devourVoteCommandErrorsWhenCallerHasVoted,
    testProperty "PROP: devour vote command errors when target werewolf" prop_devourVoteCommandErrorsWhenTargetWerewolf,
    testProperty "PROP: devour vote command updates votes" prop_devourVoteCommandUpdatesVotes,

    testProperty "PROP: lynch vote command errors when game is over" prop_lynchVoteCommandErrorsWhenGameIsOver,
    testProperty "PROP: lynch vote command errors when caller does not exist" prop_lynchVoteCommandErrorsWhenCallerDoesNotExist,
    testProperty "PROP: lynch vote command errors when target does not exist" prop_lynchVoteCommandErrorsWhenTargetDoesNotExist,
    testProperty "PROP: lynch vote command errors when caller is dead" prop_lynchVoteCommandErrorsWhenCallerIsDead,
    testProperty "PROP: lynch vote command errors when target is dead" prop_lynchVoteCommandErrorsWhenTargetIsDead,
    testProperty "PROP: lynch vote command errors when not villages turn" prop_lynchVoteCommandErrorsWhenNotVillagesTurn,
    testProperty "PROP: lynch vote command errors when caller has voted" prop_lynchVoteCommandErrorsWhenCallerHasVoted,
    testProperty "PROP: lynch vote command updates votes" prop_lynchVoteCommandUpdatesVotes,

    testProperty "PROP: quit command errors when game is over" prop_quitCommandErrorsWhenGameIsOver,
    testProperty "PROP: quit command errors when caller does not exist" prop_quitCommandErrorsWhenCallerDoesNotExist,
    testProperty "PROP: quit command errors when caller is dead" prop_quitCommandErrorsWhenCallerIsDead,
    testProperty "PROP: quit command kills player" prop_quitCommandKillsPlayer,
    testProperty "PROP: quit command clears players devour vote" prop_quitCommandClearsPlayersDevourVote,
    testProperty "PROP: quit command clears players lynch vote" prop_quitCommandClearsPlayersLynchVote,
    testProperty "PROP: quit command clears players see" prop_quitCommandClearsPlayersSee,

    testProperty "PROP: see command errors when game is over" prop_seeCommandErrorsWhenGameIsOver,
    testProperty "PROP: see command errors when caller does not exist" prop_seeCommandErrorsWhenCallerDoesNotExist,
    testProperty "PROP: see command errors when target does not exist" prop_seeCommandErrorsWhenTargetDoesNotExist,
    testProperty "PROP: see command errors when caller is dead" prop_seeCommandErrorsWhenCallerIsDead,
    testProperty "PROP: see command errors when target is dead" prop_seeCommandErrorsWhenTargetIsDead,
    testProperty "PROP: see command errors when not seers turn" prop_seeCommandErrorsWhenNotSeersTurn,
    testProperty "PROP: see command errors when caller not seer" prop_seeCommandErrorsWhenCallerNotSeer,
    testProperty "PROP: see command sets see" prop_seeCommandSetsSee
    ]

allEngineTests :: [TestTree]
allEngineTests = [
    testProperty "PROP: check stage skips seers when no seers" prop_checkStageSkipsSeersWhenNoSeers,
    testProperty "PROP: check stage does nothing when game over" prop_checkStageDoesNothingWhenGameOver,

    testProperty "PROP: check seers turn advances to werewolves" prop_checkSeersTurnAdvancesToWerewolves,
    testProperty "PROP: check seers turn resets sees" prop_checkSeersTurnResetsSee,
    testProperty "PROP: check seers turn does nothing unless all seen" prop_checkSeersTurnDoesNothingUnlessAllSeen,

    testProperty "PROP: check villages turn advances to seers" prop_checkVillagesTurnAdvancesToSeers,
    testProperty "PROP: check villages turn lynches one player when consensus" prop_checkVillagesTurnLynchesOnePlayerWhenConsensus,
    testProperty "PROP: check villages turn lynches no one when conflicted and no scapegoats" prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats,
    testProperty "PROP: check villages turn lynches scapegoat when conflicted" prop_checkVillagesTurnLynchesScapegoatWhenConflicted,
    testProperty "PROP: check villages turn resets votes" prop_checkVillagesTurnResetsVotes,
    testProperty "PROP: check villages turn does nothing unless all voted" prop_checkVillagesTurnDoesNothingUnlessAllVoted,

    testProperty "PROP: check werewolves turn advances to villages" prop_checkWerewolvesTurnAdvancesToVillages,
    testProperty "PROP: check werewolves turn kills one player when consensus" prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus,
    testProperty "PROP: check werewolves turn kills no one when conflicted" prop_checkWerewolvesTurnKillsNoOneWhenConflicted,
    testProperty "PROP: check werewolves turn resets votes" prop_checkWerewolvesTurnResetsVotes,
    testProperty "PROP: check werewolves turn does nothing unless all voted" prop_checkWerewolvesTurnDoesNothingUnlessAllVoted,

    testProperty "PROP: check game over advances stage" prop_checkGameOverAdvancesStage,
    testProperty "PROP: check game over does nothing when at least two allegiances alive" prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive,

    testProperty "PROP: start game starts with sunset stage" prop_startGameStartsWithSunsetStage,
    testProperty "PROP: start game uses given players" prop_startGameUsesGivenPlayers,
    testProperty "PROP: start game errors unless unique player names" prop_startGameErrorsUnlessUniquePlayerNames,
    testProperty "PROP: start game errors when less than 7 players" prop_startGameErrorsWhenLessThan7Players,
    testProperty "PROP: start game errors when more than 24 players" prop_startGameErrorsWhenMoreThan24Players,
    testProperty "PROP: start game errors when more than 1 seer" prop_startGameErrorsWhenMoreThan1Seer,
    testProperty "PROP: start game errors when more than 1 scapegoat" prop_startGameErrorsWhenMoreThan1Scapegoat,

    testProperty "PROP: create players uses given player names" prop_createPlayersUsesGivenPlayerNames,
    testProperty "PROP: create players uses given roles" prop_createPlayersUsesGivenRoles,
    testProperty "PROP: create players creates alive players" prop_createPlayersCreatesAlivePlayers,

    testProperty "PROP: randomise roles returns n roles" prop_randomiseRolesReturnsNRoles,
    testProperty "PROP: randomise roles uses given roles" prop_randomiseRolesUsesGivenRoles,
    testProperty "PROP: randomise roles proportions allegiances" prop_randomiseRolesProportionsAllegiances
    ]

allGameTests :: [TestTree]
allGameTests = [
    testProperty "PROP: new game starts with sunset stage" prop_newGameStartsWithSunsetStage,
    testProperty "PROP: new game starts with no see" prop_newGameStartsWithNoSee,
    testProperty "PROP: new game starts with votes empty" prop_newGameStartsWithVotesEmpty,
    testProperty "PROP: new game uses given players" prop_newGameUsesGivenPlayers
    ]

allPlayerTests :: [TestTree]
allPlayerTests = [
    testProperty "PROP: new player is alive" prop_newPlayerIsAlive
    ]
