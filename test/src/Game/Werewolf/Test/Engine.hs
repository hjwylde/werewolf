{-|
Module      : Game.Werewolf.Test.Engine
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Engine (
    -- * advanceTurn
    prop_advanceTurnSkipsSeersWhenNoSeers, prop_advanceTurnDoesNothingWhenGameOver,
    prop_advanceSeersTurnAdvancesToWerewolves, prop_advanceSeersTurnResetsSees,
    prop_advanceSeersTurnDoesNothingUnlessAllSeen, prop_advanceVillagersTurnAdvancesToSeers,
    prop_advanceVillagersTurnLynchesOnePlayerWhenConsensus,
    prop_advanceVillagersTurnLynchesNoOneWhenConflicted, prop_advanceVillagersTurnResetsVotes,
    prop_advanceVillagersTurnDoesNothingUnlessAllVoted,
    prop_advanceWerewolvesTurnAdvancesToVillagers,
    prop_advanceWerewolvesTurnKillsOnePlayerWhenConsensus,
    prop_advanceWerewolvesTurnKillsNoOneWhenConflicted, prop_advanceWerewolvesTurnResetsVotes,
    prop_advanceWerewolvesTurnDoesNothingUnlessAllVoted,

    -- * checkGameOver
    prop_checkGameOverAdvancesTurn, prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive,

    -- * startGame
    prop_startGameStartsWithSeersTurn, prop_startGameUsesGivenPlayers,
    prop_startGameErrorsUnlessUniquePlayerNames, prop_startGameErrorsWhenLessThan7Players,
    prop_startGameErrorsWhenMoreThan24Players,

    -- * createPlayers
    prop_createPlayersUsesGivenPlayerNames, prop_createPlayersCreatesAlivePlayers,

    -- * randomiseRoles
    prop_randomiseRolesReturnsNRoles, prop_randomiseRolesProportionsRoles,
    prop_randomiseRolesHasOneSeer,
) where

import Control.Lens         hiding (elements)
import Control.Monad.Except

import           Data.Either.Extra
import           Data.List.Extra
import qualified Data.Map          as Map
import           Data.Text         (Text)

import Game.Werewolf.Engine         hiding (doesPlayerExist, isGameOver, isSeersTurn,
                                     isVillagersTurn, isWerewolvesTurn, killPlayer)
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Role           hiding (Villagers, Werewolves, _name)
import Game.Werewolf.Test.Arbitrary

import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_advanceTurnSkipsSeersWhenNoSeers :: Game -> Property
prop_advanceTurnSkipsSeersWhenNoSeers game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ advanceTurn game''
    where
        game'   = (foldl killPlayer game (filterSeers $ game ^. players)) { _turn = Villagers }
        n       = length . filterAlive $ game' ^. players

prop_advanceTurnDoesNothingWhenGameOver :: Game -> Property
prop_advanceTurnDoesNothingWhenGameOver game = run_ advanceTurn game' === game'
    where
        game' = game { _turn = NoOne }

prop_advanceSeersTurnAdvancesToWerewolves :: Game -> Property
prop_advanceSeersTurnAdvancesToWerewolves game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ advanceTurn game''
    where
        game'   = game { _turn = Seers }
        n       = length . filterSeers $ game' ^. players

prop_advanceSeersTurnResetsSees :: Game -> Property
prop_advanceSeersTurnResetsSees game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    Map.null $ run_ advanceTurn game'' ^. sees
    where
        game'   = game { _turn = Seers }
        n       = length . filterSeers $ game' ^. players

prop_advanceSeersTurnDoesNothingUnlessAllSeen :: Game -> Property
prop_advanceSeersTurnDoesNothingUnlessAllSeen game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isSeersTurn $ run_ advanceTurn game''
    where
        game'   = game { _turn = Seers }
        n       = length (filterSeers $ game' ^. players) - 1

prop_advanceVillagersTurnAdvancesToSeers :: Game -> Property
prop_advanceVillagersTurnAdvancesToSeers game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    not (null . filterAlive . filterSeers $ run_ advanceTurn game'' ^. players)
    ==> isSeersTurn $ run_ advanceTurn game''
    where
        game'   = game { _turn = Villagers }
        n       = length $ game' ^. players

prop_advanceVillagersTurnLynchesOnePlayerWhenConsensus :: Game -> Property
prop_advanceVillagersTurnLynchesOnePlayerWhenConsensus game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) == 1
    ==> length (filterDead $ run_ advanceTurn game'' ^. players) == 1
    where
        game'   = game { _turn = Villagers }
        n       = length $ game' ^. players

prop_advanceVillagersTurnLynchesNoOneWhenConflicted :: Game -> Property
prop_advanceVillagersTurnLynchesNoOneWhenConflicted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) > 1
    ==> length (filterDead $ run_ advanceTurn game'' ^. players) == 0
    where
        game'   = game { _turn = Villagers }
        n       = length $ game' ^. players

prop_advanceVillagersTurnResetsVotes :: Game -> Property
prop_advanceVillagersTurnResetsVotes game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    Map.null $ run_ advanceTurn game'' ^. votes
    where
        game'   = game { _turn = Villagers }
        n       = length $ game' ^. players

prop_advanceVillagersTurnDoesNothingUnlessAllVoted :: Game -> Property
prop_advanceVillagersTurnDoesNothingUnlessAllVoted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isVillagersTurn $ run_ advanceTurn game''
    where
        game'   = game { _turn = Villagers }
        n       = length (game' ^. players) - 1

prop_advanceWerewolvesTurnAdvancesToVillagers :: Game -> Property
prop_advanceWerewolvesTurnAdvancesToVillagers game =
    forAll (runArbitraryCommands n game') $ \game' ->
    isVillagersTurn $ run_ advanceTurn game'
    where
        game'   = game { _turn = Werewolves }
        n       = length . filterWerewolves $ game' ^. players

prop_advanceWerewolvesTurnKillsOnePlayerWhenConsensus :: Game -> Property
prop_advanceWerewolvesTurnKillsOnePlayerWhenConsensus game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) == 1
    ==> length (filterDead $ run_ advanceTurn game'' ^. players) == 1
    where
        game'   = game { _turn = Werewolves }
        n       = length $ game' ^. players

prop_advanceWerewolvesTurnKillsNoOneWhenConflicted :: Game -> Property
prop_advanceWerewolvesTurnKillsNoOneWhenConflicted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) > 1
    ==> length (filterDead $ run_ advanceTurn game'' ^. players) == 0
    where
        game'   = game { _turn = Werewolves }
        n       = length $ game' ^. players

prop_advanceWerewolvesTurnResetsVotes :: Game -> Property
prop_advanceWerewolvesTurnResetsVotes game =
    forAll (runArbitraryCommands n game') $ \game' ->
    Map.null $ run_ advanceTurn game' ^. votes
    where
        game'   = game { _turn = Werewolves }
        n       = length . filterWerewolves $ game' ^. players

prop_advanceWerewolvesTurnDoesNothingUnlessAllVoted :: Game -> Property
prop_advanceWerewolvesTurnDoesNothingUnlessAllVoted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ advanceTurn game''
    where
        game'   = game { _turn = Werewolves }
        n       = length (filterWerewolves $ game' ^. players) - 1

prop_checkGameOverAdvancesTurn :: Game -> Property
prop_checkGameOverAdvancesTurn game =
    forAll (sublistOf $ game ^. players) $ \players' ->
    let game' = foldl killPlayer game players' in
        length (nub . map (_allegiance . _role) . filterAlive $ game' ^. players) <= 1
        ==> isGameOver $ run_ checkGameOver game'

prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive :: Game -> Property
prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive game =
    not (isGameOver game)
    ==> forAll (sublistOf $ game ^. players) $ \players' ->
        let game' = foldl killPlayer game players' in
            length (nub . map (_allegiance . _role) . filterAlive $ game' ^. players) > 1
            ==> not . isGameOver $ run_ checkGameOver game'

prop_startGameStartsWithSeersTurn :: [Player] -> Property
prop_startGameStartsWithSeersTurn players = and [
    isRight . runExcept $ startGame "" players
    ] ==> isSeersTurn (fromRight . runExcept $ startGame "" players)

prop_startGameUsesGivenPlayers :: [Player] -> Property
prop_startGameUsesGivenPlayers players' = and [
    isRight . runExcept $ startGame "" players'
    ] ==> (fromRight . runExcept $ startGame "" players') ^. players == players'

prop_startGameErrorsUnlessUniquePlayerNames :: [Player] -> Property
prop_startGameErrorsUnlessUniquePlayerNames players = and [
    isRight . runExcept $ startGame "" players
    ] ==> forAll (elements players) $ \player -> isLeft (runExcept $ startGame "" (player:players))

prop_startGameErrorsWhenLessThan7Players :: [Player] -> Property
prop_startGameErrorsWhenLessThan7Players players = and [
    length players < 7
    ] ==> isLeft (runExcept $ startGame "" players)

prop_startGameErrorsWhenMoreThan24Players :: Property
prop_startGameErrorsWhenMoreThan24Players = forAll (resize 30 $ listOf arbitrary) $ \players -> and [
    length players > 24
    ] ==> isLeft (runExcept $ startGame "" players)

prop_createPlayersUsesGivenPlayerNames :: [Text] -> Property
prop_createPlayersUsesGivenPlayerNames playerNames = monadicIO $ createPlayers playerNames >>= return . (playerNames ==) . map _name

prop_createPlayersCreatesAlivePlayers :: [Text] -> Property
prop_createPlayersCreatesAlivePlayers playerNames = monadicIO $ createPlayers playerNames >>= return . all ((==) Alive . _state)

prop_randomiseRolesReturnsNRoles :: Int -> Property
prop_randomiseRolesReturnsNRoles n = monadicIO $ randomiseRoles n >>= return . (==) n . length

prop_randomiseRolesProportionsRoles :: Int -> Property
prop_randomiseRolesProportionsRoles n = monadicIO $ do
    roles <- randomiseRoles n

    let werewolvesCount = length $ elemIndices werewolfRole roles

    return $ n `quot` 6 + 1 == werewolvesCount

prop_randomiseRolesHasOneSeer :: Int -> Property
prop_randomiseRolesHasOneSeer n = monadicIO $ randomiseRoles n >>= return . (1 ==) . length . elemIndices seerRole
