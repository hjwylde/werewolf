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
import Game.Werewolf.Test.Engine
import Game.Werewolf.Test.Game
import Game.Werewolf.Test.Player

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = return $ testGroup "Tests" (concat [allEngineTests, allGameTests, allPlayerTests])

allEngineTests :: [TestTree]
allEngineTests = [
    testProperty "PROP: validate vote command errors when game is over" prop_validateVoteCommandErrorsWhenGameIsOver,
    testProperty "PROP: validate vote command errors when voter not in game" prop_validateVoteCommandErrorsWhenVoterDoesNotExist,
    testProperty "PROP: validate vote command errors when target not in game" prop_validateVoteCommandErrorsWhenTargetDoesNotExist,
    testProperty "PROP: validate vote command errors when voter is dead" prop_validateVoteCommandErrorsWhenVoterIsDead,
    testProperty "PROP: validate vote command errors when target is dead" prop_validateVoteCommandErrorsWhenTargetIsDead,
    testProperty "PROP: validate vote command errors when voter has voted" prop_validateVoteCommandErrorsWhenVoterHasVoted,
    testProperty "PROP: validate kill vote command errors when voter not werewolf" prop_validateKillVoteCommandErrorsWhenVoterNotWerewolf,

    testProperty "PROP: start game starts with werewolves turn" prop_startGameStartsWithWerewolvesTurn,
    testProperty "PROP: start game errors unless unique player names" prop_startGameErrorsUnlessUniquePlayerNames,
    testProperty "PROP: start game errors when less than 7 players" prop_startGameErrorsWhenLessThan7Players,
    testProperty "PROP: start game errors when more than 24 players" prop_startGameErrorsWhenMoreThan24Players,

    testProperty "PROP: create players uses given player names" prop_createPlayersUsesGivenPlayerNames,
    testProperty "PROP: create players creates alive players" prop_createPlayersCreatesAlivePlayers,

    testProperty "PROP: randomise roles returns n roles" prop_randomiseRolesReturnsNRoles
    ]

allGameTests :: [TestTree]
allGameTests = [
    testProperty "PROP: new game starts with werewolves turn" prop_newGameStartsWithWerewolvesTurn,
    testCase "CHCK: newVillagersTurn" check_newVillagersTurn,
    testCase "CHCK: newWerewolvesTurn" check_newWerewolvesTurn
    ]

allPlayerTests :: [TestTree]
allPlayerTests = [testProperty "PROP: new player is alive" prop_newPlayerIsAlive]
