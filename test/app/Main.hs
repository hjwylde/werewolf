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
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = return $ testGroup "Tests" (concat [allCommandTests, allEngineTests, allGameTests, allPlayerTests])

allCommandTests :: [TestTree]
allCommandTests = [
    testProperty "PROP: see command errors when game is over" prop_seeCommandErrorsWhenGameIsOver,
    testProperty "PROP: see command errors when caller does not exist" prop_seeCommandErrorsWhenCallerDoesNotExist,
    testProperty "PROP: see command errors when target does not exist" prop_seeCommandErrorsWhenTargetDoesNotExist,
    testProperty "PROP: see command errors when caller is dead" prop_seeCommandErrorsWhenCallerIsDead,
    testProperty "PROP: see command errors when target is dead" prop_seeCommandErrorsWhenTargetIsDead,
    testProperty "PROP: see command errors when not seers turn" prop_seeCommandErrorsWhenNotSeersTurn,
    testProperty "PROP: see command errors when caller not seer" prop_seeCommandErrorsWhenCallerNotSeer,
    testProperty "PROP: see command errors when caller has seen" prop_seeCommandErrorsWhenCallerHasSeen,
    testProperty "PROP: see command errors updates sees" prop_seeCommandUpdatesSees,

    testProperty "PROP: kill vote command errors when game is over" prop_killVoteCommandErrorsWhenGameIsOver,
    testProperty "PROP: kill vote command errors when caller does not exist" prop_killVoteCommandErrorsWhenCallerDoesNotExist,
    testProperty "PROP: kill vote command errors when target does not exist" prop_killVoteCommandErrorsWhenTargetDoesNotExist,
    testProperty "PROP: kill vote command errors when caller is dead" prop_killVoteCommandErrorsWhenCallerIsDead,
    testProperty "PROP: kill vote command errors when target is dead" prop_killVoteCommandErrorsWhenTargetIsDead,
    testProperty "PROP: kill vote command errors when not werewolves turn" prop_killVoteCommandErrorsWhenNotWerewolvesTurn,
    testProperty "PROP: kill vote command errors when caller not werewolf" prop_killVoteCommandErrorsWhenCallerNotWerewolf,
    testProperty "PROP: kill vote command errors when caller has voted" prop_killVoteCommandErrorsWhenCallerHasVoted,
    testProperty "PROP: kill vote command updates votes" prop_killVoteCommandUpdatesVotes,

    testProperty "PROP: lynch vote command errors when game is over" prop_lynchVoteCommandErrorsWhenGameIsOver,
    testProperty "PROP: lynch vote command errors when caller does not exist" prop_lynchVoteCommandErrorsWhenCallerDoesNotExist,
    testProperty "PROP: lynch vote command errors when target does not exist" prop_lynchVoteCommandErrorsWhenTargetDoesNotExist,
    testProperty "PROP: lynch vote command errors when caller is dead" prop_lynchVoteCommandErrorsWhenCallerIsDead,
    testProperty "PROP: lynch vote command errors when target is dead" prop_lynchVoteCommandErrorsWhenTargetIsDead,
    testProperty "PROP: lynch vote command errors when not villagers turn" prop_lynchVoteCommandErrorsWhenNotVillagersTurn,
    testProperty "PROP: lynch vote command errors when caller has voted" prop_lynchVoteCommandErrorsWhenCallerHasVoted,
    testProperty "PROP: lynch vote command updates votes" prop_lynchVoteCommandUpdatesVotes
    ]

allEngineTests :: [TestTree]
allEngineTests = [
    testProperty "PROP: start game starts with seers turn" prop_startGameStartsWithSeersTurn,
    testProperty "PROP: start game uses given players" prop_startGameUsesGivenPlayers,
    testProperty "PROP: start game errors unless unique player names" prop_startGameErrorsUnlessUniquePlayerNames,
    testProperty "PROP: start game errors when less than 7 players" prop_startGameErrorsWhenLessThan7Players,
    testProperty "PROP: start game errors when more than 24 players" prop_startGameErrorsWhenMoreThan24Players,

    testProperty "PROP: create players uses given player names" prop_createPlayersUsesGivenPlayerNames,
    testProperty "PROP: create players creates alive players" prop_createPlayersCreatesAlivePlayers,

    testProperty "PROP: randomise roles returns n roles" prop_randomiseRolesReturnsNRoles
    ]

allGameTests :: [TestTree]
allGameTests = [
    testProperty "PROP: new game starts with seers turn" prop_newGameStartsWithSeersTurn,
    testProperty "PROP: new game uses given players" prop_newGameUsesGivenPlayers,
    testCase "CHCK: newSeersTurn" check_newSeersTurn,
    testCase "CHCK: newVillagersTurn" check_newVillagersTurn,
    testCase "CHCK: newWerewolvesTurn" check_newWerewolvesTurn
    ]

allPlayerTests :: [TestTree]
allPlayerTests = [testProperty "PROP: new player is alive" prop_newPlayerIsAlive]
