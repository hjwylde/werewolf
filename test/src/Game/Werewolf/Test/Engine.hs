{-|
Module      : Game.Werewolf.Test.Engine
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Engine (
    -- * checkTurn
    prop_checkTurnSkipsSeersWhenNoSeers, prop_checkTurnDoesNothingWhenGameOver,
    prop_checkSeersTurnAdvancesToWerewolves, prop_checkSeersTurnResetsSees,
    prop_checkSeersTurnDoesNothingUnlessAllSeen, prop_checkVillagersTurnAdvancesToSeers,
    prop_checkVillagersTurnLynchesOnePlayerWhenConsensus, prop_checkVillagersTurnLynchesNoOneWhenConflictedAndNoScapegoats,
    prop_checkVillagersTurnLynchesScapegoatWhenConflicted, prop_checkVillagersTurnResetsVotes,
    prop_checkVillagersTurnDoesNothingUnlessAllVoted, prop_checkWerewolvesTurnAdvancesToVillagers,
    prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus,
    prop_checkWerewolvesTurnKillsNoOneWhenConflicted, prop_checkWerewolvesTurnResetsVotes,
    prop_checkWerewolvesTurnDoesNothingUnlessAllVoted,

    -- * checkGameOver
    prop_checkGameOverAdvancesTurn, prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive,

    -- * startGame
    prop_startGameStartsWithNightfallTurn, prop_startGameUsesGivenPlayers,
    prop_startGameErrorsUnlessUniquePlayerNames, prop_startGameErrorsWhenLessThan7Players,
    prop_startGameErrorsWhenMoreThan24Players,

    -- * createPlayers
    prop_createPlayersUsesGivenPlayerNames, prop_createPlayersUsesGivenRoles,
    prop_createPlayersCreatesAlivePlayers,

    -- * randomiseRoles
    prop_randomiseRolesReturnsNRoles, prop_randomiseRolesUsesGivenRoles,
    prop_randomiseRolesProportionsAllegiances,
) where

import Control.Lens         hiding (elements)
import Control.Monad.Except
import Control.Monad.Writer

import           Data.Either.Extra
import           Data.List.Extra
import qualified Data.Map          as Map
import           Data.Text         (Text)

import           Game.Werewolf.Engine         hiding (doesPlayerExist, isGameOver, isSeersTurn,
                                               isVillagersTurn, isWerewolvesTurn, killPlayer)
import           Game.Werewolf.Game
import           Game.Werewolf.Player
import           Game.Werewolf.Role           hiding (Villagers, Werewolves, _name)
import qualified Game.Werewolf.Role           as Role
import           Game.Werewolf.Test.Arbitrary

import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_checkTurnSkipsSeersWhenNoSeers :: Game -> Property
prop_checkTurnSkipsSeersWhenNoSeers game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ checkTurn game''
    where
        game'   = (foldl killPlayer game (filterSeers $ game ^. players)) { _turn = Villagers }
        n       = length . filterAlive $ game' ^. players

prop_checkTurnDoesNothingWhenGameOver :: Game -> Property
prop_checkTurnDoesNothingWhenGameOver game = run_ checkTurn game' === game'
    where
        game' = game { _turn = NoOne }

prop_checkSeersTurnAdvancesToWerewolves :: Game -> Property
prop_checkSeersTurnAdvancesToWerewolves game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ checkTurn game''
    where
        game'   = game { _turn = Seers }
        n       = length . filterSeers $ game' ^. players

prop_checkSeersTurnResetsSees :: Game -> Property
prop_checkSeersTurnResetsSees game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    Map.null $ run_ checkTurn game'' ^. sees
    where
        game'   = game { _turn = Seers }
        n       = length . filterSeers $ game' ^. players

prop_checkSeersTurnDoesNothingUnlessAllSeen :: Game -> Property
prop_checkSeersTurnDoesNothingUnlessAllSeen game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isSeersTurn $ run_ checkTurn game''
    where
        game'   = game { _turn = Seers }
        n       = length (filterSeers $ game' ^. players) - 1

prop_checkVillagersTurnAdvancesToSeers :: Game -> Property
prop_checkVillagersTurnAdvancesToSeers game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    not (null . filterAlive . filterSeers $ run_ checkTurn game'' ^. players)
    ==> isSeersTurn $ run_ checkTurn game''
    where
        game'   = game { _turn = Villagers }
        n       = length $ game' ^. players

prop_checkVillagersTurnLynchesOnePlayerWhenConsensus :: Game -> Property
prop_checkVillagersTurnLynchesOnePlayerWhenConsensus game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) == 1
    ==> length (filterDead $ run_ checkTurn game'' ^. players) == 1
    where
        game'   = game { _turn = Villagers }
        n       = length $ game' ^. players

prop_checkVillagersTurnLynchesNoOneWhenConflictedAndNoScapegoats :: Game -> Property
prop_checkVillagersTurnLynchesNoOneWhenConflictedAndNoScapegoats game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) > 1
    ==> length (filterDead $ run_ checkTurn game'' ^. players) == length (filterDead $ game' ^. players)
    where
        game'   = (foldl killPlayer game (filterScapegoats $ game ^. players)) { _turn = Villagers }
        n       = length $ game' ^. players

prop_checkVillagersTurnLynchesScapegoatWhenConflicted :: Game -> Property
prop_checkVillagersTurnLynchesScapegoatWhenConflicted game =
    forAll (runArbitraryCommands n game') $ \game'' -> and [
        length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) > 1,
        any isScapegoat $ game' ^. players
        ] ==> isScapegoat $ head (filterDead $ run_ checkTurn game'' ^. players)
    where
        game'   = game { _turn = Villagers }
        n       = length $ game' ^. players

prop_checkVillagersTurnResetsVotes :: Game -> Property
prop_checkVillagersTurnResetsVotes game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    Map.null $ run_ checkTurn game'' ^. votes
    where
        game'   = game { _turn = Villagers }
        n       = length $ game' ^. players

prop_checkVillagersTurnDoesNothingUnlessAllVoted :: Game -> Property
prop_checkVillagersTurnDoesNothingUnlessAllVoted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isVillagersTurn $ run_ checkTurn game''
    where
        game'   = game { _turn = Villagers }
        n       = length (game' ^. players) - 1

prop_checkWerewolvesTurnAdvancesToVillagers :: Game -> Property
prop_checkWerewolvesTurnAdvancesToVillagers game =
    forAll (runArbitraryCommands n game') $ \game' ->
    isVillagersTurn $ run_ checkTurn game'
    where
        game'   = game { _turn = Werewolves }
        n       = length . filterWerewolves $ game' ^. players

prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus :: Game -> Property
prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) == 1
    ==> length (filterDead $ run_ checkTurn game'' ^. players) == 1
    where
        game'   = game { _turn = Werewolves }
        n       = length $ game' ^. players

prop_checkWerewolvesTurnKillsNoOneWhenConflicted :: Game -> Property
prop_checkWerewolvesTurnKillsNoOneWhenConflicted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) > 1
    ==> length (filterDead $ run_ checkTurn game'' ^. players) == 0
    where
        game'   = game { _turn = Werewolves }
        n       = length $ game' ^. players

prop_checkWerewolvesTurnResetsVotes :: Game -> Property
prop_checkWerewolvesTurnResetsVotes game =
    forAll (runArbitraryCommands n game') $ \game' ->
    Map.null $ run_ checkTurn game' ^. votes
    where
        game'   = game { _turn = Werewolves }
        n       = length . filterWerewolves $ game' ^. players

prop_checkWerewolvesTurnDoesNothingUnlessAllVoted :: Game -> Property
prop_checkWerewolvesTurnDoesNothingUnlessAllVoted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ checkTurn game''
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

prop_startGameStartsWithNightfallTurn :: [Player] -> Property
prop_startGameStartsWithNightfallTurn players = and [
    any isSeer players,
    isRight . runExcept . runWriterT $ startGame "" players
    ] ==> (fst . fromRight . runExcept . runWriterT $ startGame "" players) ^. turn == NightFalling

prop_startGameUsesGivenPlayers :: [Player] -> Property
prop_startGameUsesGivenPlayers players' = and [
    isRight . runExcept . runWriterT $ startGame "" players'
    ] ==> (fst . fromRight . runExcept . runWriterT $ startGame "" players') ^. players == players'

prop_startGameErrorsUnlessUniquePlayerNames :: [Player] -> Property
prop_startGameErrorsUnlessUniquePlayerNames players = and [
    isRight . runExcept . runWriterT $ startGame "" players
    ] ==> forAll (elements players) $ \player -> isLeft (runExcept . runWriterT $ startGame "" (player:players))

prop_startGameErrorsWhenLessThan7Players :: [Player] -> Property
prop_startGameErrorsWhenLessThan7Players players = and [
    length players < 7
    ] ==> isLeft (runExcept . runWriterT $ startGame "" players)

prop_startGameErrorsWhenMoreThan24Players :: Property
prop_startGameErrorsWhenMoreThan24Players = forAll (resize 30 $ listOf arbitrary) $ \players -> and [
    length players > 24
    ] ==> isLeft (runExcept . runWriterT $ startGame "" players)

prop_createPlayersUsesGivenPlayerNames :: [Text] -> [Role] -> Property
prop_createPlayersUsesGivenPlayerNames playerNames extraRoles = monadicIO $ createPlayers playerNames extraRoles >>= return . (playerNames ==) . map _name

prop_createPlayersUsesGivenRoles :: [Text] -> [Role] -> Property
prop_createPlayersUsesGivenRoles playerNames extraRoles = monadicIO $ createPlayers playerNames extraRoles >>= return . isSubsequenceOf extraRoles . map _role

prop_createPlayersCreatesAlivePlayers :: [Text] -> [Role] -> Property
prop_createPlayersCreatesAlivePlayers playerNames extraRoles = monadicIO $ createPlayers playerNames extraRoles >>= return . all ((==) Alive . _state)

prop_randomiseRolesReturnsNRoles :: [Role] -> Int -> Property
prop_randomiseRolesReturnsNRoles extraRoles n = monadicIO $ randomiseRoles extraRoles n >>= return . (==) n . length

prop_randomiseRolesUsesGivenRoles :: [Role] -> Int -> Property
prop_randomiseRolesUsesGivenRoles extraRoles n = monadicIO $ randomiseRoles extraRoles n >>= return . isSubsequenceOf extraRoles

prop_randomiseRolesProportionsAllegiances :: [Role] -> Int -> Property
prop_randomiseRolesProportionsAllegiances extraRoles n = monadicIO $ do
    roles <- randomiseRoles extraRoles n

    let werewolvesCount = length . elemIndices Role.Werewolves $ map _allegiance roles

    return $ n `quot` 6 + 1 == werewolvesCount
