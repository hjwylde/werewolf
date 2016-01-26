{-|
Module      : Game.Werewolf.Test.Engine
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Engine (
    -- * checkStage
    prop_checkStageSkipsSeersWhenNoSeers, prop_checkStageDoesNothingWhenGameOver,
    prop_checkSeersTurnAdvancesToWerewolves, prop_checkSeersTurnResetsSees,
    prop_checkSeersTurnDoesNothingUnlessAllSeen, prop_checkVillagesTurnAdvancesToSeers,
    prop_checkVillagesTurnLynchesOnePlayerWhenConsensus, prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats,
    prop_checkVillagesTurnLynchesScapegoatWhenConflicted, prop_checkVillagesTurnResetsVotes,
    prop_checkVillagesTurnDoesNothingUnlessAllVoted, prop_checkWerewolvesTurnAdvancesToVillages,
    prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus,
    prop_checkWerewolvesTurnKillsNoOneWhenConflicted, prop_checkWerewolvesTurnResetsVotes,
    prop_checkWerewolvesTurnDoesNothingUnlessAllVoted,

    -- * checkGameOver
    prop_checkGameOverAdvancesStage, prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive,

    -- * startGame
    prop_startGameStartsWithSunsetStage, prop_startGameUsesGivenPlayers,
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
                                               isVillagesTurn, isWerewolvesTurn, killPlayer)
import           Game.Werewolf.Game
import           Game.Werewolf.Player
import           Game.Werewolf.Role           hiding (_name)
import qualified Game.Werewolf.Role           as Role
import           Game.Werewolf.Test.Arbitrary

import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_checkStageSkipsSeersWhenNoSeers :: Game -> Property
prop_checkStageSkipsSeersWhenNoSeers game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ checkStage game''
    where
        game'   = (foldl killPlayer game (filterSeers $ game ^. players)) { _stage = VillagesTurn }
        n       = length . filterAlive $ game' ^. players

prop_checkStageDoesNothingWhenGameOver :: Game -> Property
prop_checkStageDoesNothingWhenGameOver game = run_ checkStage game' === game'
    where
        game' = game { _stage = GameOver }

prop_checkSeersTurnAdvancesToWerewolves :: Game -> Property
prop_checkSeersTurnAdvancesToWerewolves game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ checkStage game''
    where
        game'   = game { _stage = SeersTurn }
        n       = length . filterSeers $ game' ^. players

prop_checkSeersTurnResetsSees :: Game -> Property
prop_checkSeersTurnResetsSees game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    Map.null $ run_ checkStage game'' ^. sees
    where
        game'   = game { _stage = SeersTurn }
        n       = length . filterSeers $ game' ^. players

prop_checkSeersTurnDoesNothingUnlessAllSeen :: Game -> Property
prop_checkSeersTurnDoesNothingUnlessAllSeen game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isSeersTurn $ run_ checkStage game''
    where
        game'   = game { _stage = SeersTurn }
        n       = length (filterSeers $ game' ^. players) - 1

prop_checkVillagesTurnAdvancesToSeers :: Game -> Property
prop_checkVillagesTurnAdvancesToSeers game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    not (null . filterAlive . filterSeers $ run_ checkStage game'' ^. players)
    ==> isSeersTurn $ run_ checkStage game''
    where
        game'   = game { _stage = VillagesTurn }
        n       = length $ game' ^. players

prop_checkVillagesTurnLynchesOnePlayerWhenConsensus :: Game -> Property
prop_checkVillagesTurnLynchesOnePlayerWhenConsensus game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) == 1
    ==> length (filterDead $ run_ checkStage game'' ^. players) == 1
    where
        game'   = game { _stage = VillagesTurn }
        n       = length $ game' ^. players

prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats :: Game -> Property
prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) > 1
    ==> length (filterDead $ run_ checkStage game'' ^. players) == length (filterDead $ game' ^. players)
    where
        game'   = (foldl killPlayer game (filterScapegoats $ game ^. players)) { _stage = VillagesTurn }
        n       = length $ game' ^. players

prop_checkVillagesTurnLynchesScapegoatWhenConflicted :: Game -> Property
prop_checkVillagesTurnLynchesScapegoatWhenConflicted game =
    forAll (runArbitraryCommands n game') $ \game'' -> and [
        length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) > 1,
        any isScapegoat $ game' ^. players
        ] ==> isScapegoat $ head (filterDead $ run_ checkStage game'' ^. players)
    where
        game'   = game { _stage = VillagesTurn }
        n       = length $ game' ^. players

prop_checkVillagesTurnResetsVotes :: Game -> Property
prop_checkVillagesTurnResetsVotes game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    Map.null $ run_ checkStage game'' ^. votes
    where
        game'   = game { _stage = VillagesTurn }
        n       = length $ game' ^. players

prop_checkVillagesTurnDoesNothingUnlessAllVoted :: Game -> Property
prop_checkVillagesTurnDoesNothingUnlessAllVoted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isVillagesTurn $ run_ checkStage game''
    where
        game'   = game { _stage = VillagesTurn }
        n       = length (game' ^. players) - 1

prop_checkWerewolvesTurnAdvancesToVillages :: Game -> Property
prop_checkWerewolvesTurnAdvancesToVillages game =
    forAll (runArbitraryCommands n game') $ \game' ->
    isVillagesTurn $ run_ checkStage game'
    where
        game'   = game { _stage = WerewolvesTurn }
        n       = length . filterWerewolves $ game' ^. players

prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus :: Game -> Property
prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) == 1
    ==> length (filterDead $ run_ checkStage game'' ^. players) == 1
    where
        game'   = game { _stage = WerewolvesTurn }
        n       = length $ game' ^. players

prop_checkWerewolvesTurnKillsNoOneWhenConflicted :: Game -> Property
prop_checkWerewolvesTurnKillsNoOneWhenConflicted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (last $ groupSortOn (length . flip elemIndices (Map.elems $ game'' ^. votes)) (nub . Map.elems $ game'' ^. votes)) > 1
    ==> length (filterDead $ run_ checkStage game'' ^. players) == 0
    where
        game'   = game { _stage = WerewolvesTurn }
        n       = length $ game' ^. players

prop_checkWerewolvesTurnResetsVotes :: Game -> Property
prop_checkWerewolvesTurnResetsVotes game =
    forAll (runArbitraryCommands n game') $ \game' ->
    Map.null $ run_ checkStage game' ^. votes
    where
        game'   = game { _stage = WerewolvesTurn }
        n       = length . filterWerewolves $ game' ^. players

prop_checkWerewolvesTurnDoesNothingUnlessAllVoted :: Game -> Property
prop_checkWerewolvesTurnDoesNothingUnlessAllVoted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ checkStage game''
    where
        game'   = game { _stage = WerewolvesTurn }
        n       = length (filterWerewolves $ game' ^. players) - 1

prop_checkGameOverAdvancesStage :: Game -> Property
prop_checkGameOverAdvancesStage game =
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

prop_startGameStartsWithSunsetStage :: [Player] -> Property
prop_startGameStartsWithSunsetStage players =
    isRight (runExcept . runWriterT $ startGame "" players)
    ==> isSunset (fst . fromRight . runExcept . runWriterT $ startGame "" players)

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
