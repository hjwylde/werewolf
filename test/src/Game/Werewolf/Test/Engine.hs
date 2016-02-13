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
    prop_checkStageSkipsDefendersTurnWhenNoDefender, prop_checkStageSkipsSeersTurnWhenNoSeer,
    prop_checkStageSkipsWitchsTurnWhenNoWitch, prop_checkStageDoesNothingWhenGameOver,

    prop_checkDefendersTurnAdvancesToWerewolvesTurn,

    prop_checkSeersTurnAdvancesToDefendersTurn, prop_checkSeersTurnResetsSee,
    prop_checkSeersTurnDoesNothingUnlessSeen,

    prop_checkVillagesTurnAdvancesToSeersTurn, prop_checkVillagesTurnLynchesOnePlayerWhenConsensus,
    prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats,
    prop_checkVillagesTurnLynchesScapegoatWhenConflicted, prop_checkVillagesTurnResetsVotes,
    prop_checkVillagesTurnDoesNothingUnlessAllVoted,

    prop_checkWerewolvesTurnAdvancesToWitchsTurn,
    prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned,
    prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus,
    prop_checkWerewolvesTurnKillsNoOneWhenConflicted,
    prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended, prop_checkWerewolvesTurnResetsProtect,
    prop_checkWerewolvesTurnResetsVotes, prop_checkWerewolvesTurnDoesNothingUnlessAllVoted,

    prop_checkWitchsTurnAdvancesToVillagesTurn, prop_checkWitchsTurnHealsDevoureeWhenHealed,
    prop_checkWitchsTurnKillsOnePlayerWhenPoisoned, prop_checkWitchsTurnDoesNothingWhenPassed,
    prop_checkWitchsTurnResetsHeal, prop_checkWitchsTurnResetsPoison,

    -- * checkGameOver
    prop_checkGameOverAdvancesStage, prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive,

    -- * startGame
    prop_startGameStartsWithSunsetStage, prop_startGameUsesGivenPlayers,
    prop_startGameErrorsUnlessUniquePlayerNames, prop_startGameErrorsWhenLessThan7Players,
    prop_startGameErrorsWhenMoreThan24Players, prop_startGameErrorsWhenMoreThan1Defender,
    prop_startGameErrorsWhenMoreThan1Scapegoat, prop_startGameErrorsWhenMoreThan1Seer,
    prop_startGameErrorsWhenMoreThan1VillagerVillager, prop_startGameErrorsWhenMoreThan1Witch,
    prop_startGameErrorsWhenMoreThan1WolfHound,

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
import           Data.Maybe
import           Data.Text         (Text)

import           Game.Werewolf.Command
import           Game.Werewolf.Engine         hiding (doesPlayerExist, getDevourEvent,
                                               getVoteResult, isDefendersTurn, isGameOver,
                                               isSeersTurn, isVillagesTurn, isWerewolvesTurn,
                                               isWitchsTurn, killPlayer)
import           Game.Werewolf.Game
import           Game.Werewolf.Player
import           Game.Werewolf.Role           hiding (name)
import qualified Game.Werewolf.Role           as Role
import           Game.Werewolf.Test.Arbitrary
import           Game.Werewolf.Test.Util

import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_checkStageSkipsDefendersTurnWhenNoDefender :: GameWithSee -> Bool
prop_checkStageSkipsDefendersTurnWhenNoDefender (GameWithSee game) =
    not . isDefendersTurn $ run_ checkStage game'
    where
        game' = foldl killPlayer game (filterDefenders $ game ^. players)

prop_checkStageSkipsSeersTurnWhenNoSeer :: GameWithLynchVotes -> Bool
prop_checkStageSkipsSeersTurnWhenNoSeer (GameWithLynchVotes game) =
    not . isSeersTurn $ run_ checkStage game'
    where
        game' = foldl killPlayer game (filterSeers $ game ^. players)

prop_checkStageSkipsWitchsTurnWhenNoWitch :: GameWithDevourVotes -> Bool
prop_checkStageSkipsWitchsTurnWhenNoWitch (GameWithDevourVotes game) =
    not . isWitchsTurn $ run_ checkStage game'
    where
        game' = foldl killPlayer game (filterWitches $ game ^. players)

prop_checkStageDoesNothingWhenGameOver :: GameAtGameOver -> Property
prop_checkStageDoesNothingWhenGameOver (GameAtGameOver game) =
    run_ checkStage game === game

prop_checkDefendersTurnAdvancesToWerewolvesTurn :: GameWithProtect -> Bool
prop_checkDefendersTurnAdvancesToWerewolvesTurn (GameWithProtect game) =
    isWerewolvesTurn $ run_ checkStage game

prop_checkSeersTurnAdvancesToDefendersTurn :: GameWithSee -> Bool
prop_checkSeersTurnAdvancesToDefendersTurn (GameWithSee game) =
    isDefendersTurn $ run_ checkStage game

prop_checkSeersTurnResetsSee :: GameWithSee -> Bool
prop_checkSeersTurnResetsSee (GameWithSee game) =
    isNothing $ run_ checkStage game ^. see

prop_checkSeersTurnDoesNothingUnlessSeen :: GameAtSeersTurn -> Bool
prop_checkSeersTurnDoesNothingUnlessSeen (GameAtSeersTurn game) =
    isSeersTurn $ run_ checkStage game

prop_checkVillagesTurnAdvancesToSeersTurn :: GameWithLynchVotes -> Property
prop_checkVillagesTurnAdvancesToSeersTurn (GameWithLynchVotes game) =
    any isSeer (filterAlive $ run_ checkStage game ^. players)
    ==> isSeersTurn $ run_ checkStage game

prop_checkVillagesTurnLynchesOnePlayerWhenConsensus :: GameWithLynchVotes -> Property
prop_checkVillagesTurnLynchesOnePlayerWhenConsensus (GameWithLynchVotes game) =
    length (getVoteResult game) == 1
    ==> length (filterDead $ run_ checkStage game ^. players) == 1

-- TODO (hjw)
prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats :: Game -> Property
prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') > 1
    ==> length (filterDead $ run_ checkStage game'' ^. players) == length (filterDead $ game' ^. players)
    where
        game'   = foldl killPlayer game (filterScapegoats $ game ^. players) & stage .~ VillagesTurn
        n       = length $ game' ^. players

prop_checkVillagesTurnLynchesScapegoatWhenConflicted :: GameWithLynchVotes -> Property
prop_checkVillagesTurnLynchesScapegoatWhenConflicted (GameWithLynchVotes game) =
    length (getVoteResult game) > 1
    ==> isScapegoat $ head (filterDead $ run_ checkStage game ^. players)

prop_checkVillagesTurnResetsVotes :: GameWithLynchVotes -> Bool
prop_checkVillagesTurnResetsVotes (GameWithLynchVotes game) =
    Map.null $ run_ checkStage game ^. votes

prop_checkVillagesTurnDoesNothingUnlessAllVoted :: GameAtVillagesTurn -> Property
prop_checkVillagesTurnDoesNothingUnlessAllVoted (GameAtVillagesTurn game) =
    forAll (runArbitraryCommands n game) $ \game' ->
    isVillagesTurn $ run_ checkStage game'
    where
        n = length (game ^. players) - 1

prop_checkWerewolvesTurnAdvancesToWitchsTurn :: GameWithDevourVotes -> Property
prop_checkWerewolvesTurnAdvancesToWitchsTurn (GameWithDevourVotes game) =
    length (getVoteResult game) == 1
    ==> isWitchsTurn $ run_ checkStage game

prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned :: GameWithDevourVotes -> Bool
prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned (GameWithDevourVotes game) =
    not . isWitchsTurn $ run_ checkStage game'
    where
        game' = game & healUsed .~ True & poisonUsed .~ True

prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus :: GameWithDevourVotes -> Property
prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus (GameWithDevourVotes game) =
    length (getVoteResult game) == 1
    ==> isJust . getDevourEvent $ run_ checkStage game

prop_checkWerewolvesTurnKillsNoOneWhenConflicted :: GameWithDevourVotes -> Property
prop_checkWerewolvesTurnKillsNoOneWhenConflicted (GameWithDevourVotes game) =
    length (getVoteResult game) > 1
    ==> isNothing . getDevourEvent $ run_ checkStage game

prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended :: GameWithProtectAndDevourVotes -> Property
prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended (GameWithProtectAndDevourVotes game) =
    length (getVoteResult game) == 1
    ==> isNothing . getDevourEvent $ run_ checkStage game'
    where
        target  = head $ getVoteResult game
        game'   = game & protect .~ Just (target ^. name)

prop_checkWerewolvesTurnResetsProtect :: GameWithProtectAndDevourVotes -> Bool
prop_checkWerewolvesTurnResetsProtect (GameWithProtectAndDevourVotes game) =
    isNothing $ run_ checkStage game ^. protect

prop_checkWerewolvesTurnResetsVotes :: GameWithDevourVotes -> Bool
prop_checkWerewolvesTurnResetsVotes (GameWithDevourVotes game) =
    Map.null $ run_ checkStage game ^. votes

prop_checkWerewolvesTurnDoesNothingUnlessAllVoted :: GameAtWerewolvesTurn -> Property
prop_checkWerewolvesTurnDoesNothingUnlessAllVoted (GameAtWerewolvesTurn game) =
    forAll (runArbitraryCommands n game) $ \game' ->
    isWerewolvesTurn $ run_ checkStage game'
    where
        n = length (filterWerewolves $ game ^. players) - 1

prop_checkWitchsTurnAdvancesToVillagesTurn :: GameAtWitchsTurn -> Property
prop_checkWitchsTurnAdvancesToVillagesTurn (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    isVillagesTurn $ run_ (apply command >> checkStage) game

-- TODO (hjw)
prop_checkWitchsTurnHealsDevoureeWhenHealed :: Game -> Property
prop_checkWitchsTurnHealsDevoureeWhenHealed game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') == 1
    ==> let target = head $ getVoteResult game''
        in not (isWitch target)
        ==> let game''' = run_ checkStage game''
            in forAll (arbitraryHealCommand game''') $ \(Blind command) ->
            forAll (arbitraryPassCommand game''') $ \(Blind passCommand) ->
            null . filterDead $ run_ checkStage
                (run_ (apply passCommand) $
                    run_ (apply command) game'''
                    ) ^. players
    where
        game'   = game & stage .~ WerewolvesTurn
        n       = length . filterWerewolves $ game' ^. players

prop_checkWitchsTurnKillsOnePlayerWhenPoisoned :: GameWithPoison -> Property
prop_checkWitchsTurnKillsOnePlayerWhenPoisoned (GameWithPoison game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    length (filterDead $ run_ (apply command >> checkStage) game ^. players) == 1

prop_checkWitchsTurnDoesNothingWhenPassed :: GameAtWitchsTurn -> Property
prop_checkWitchsTurnDoesNothingWhenPassed (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    null . filterDead $ run_ (apply command >> checkStage) game ^. players

prop_checkWitchsTurnResetsHeal :: GameWithHeal -> Property
prop_checkWitchsTurnResetsHeal (GameWithHeal game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    not $ run_ (apply command >> checkStage) game ^. heal

prop_checkWitchsTurnResetsPoison :: GameWithPoison -> Property
prop_checkWitchsTurnResetsPoison (GameWithPoison game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    isNothing $ run_ (apply command >> checkStage) game ^. poison

-- TODO (hjw)
prop_checkGameOverAdvancesStage :: Game -> Property
prop_checkGameOverAdvancesStage game =
    forAll (sublistOf $ game ^. players) $ \players' ->
    let game' = foldl killPlayer game players' in
        length (nub . map (view $ role . allegiance) . filterAlive $ game' ^. players) <= 1
        ==> isGameOver $ run_ checkGameOver game'

-- TODO (hjw)
prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive :: Game -> Property
prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive game =
    not (isGameOver game)
    ==> forAll (sublistOf $ game ^. players) $ \players' ->
        let game' = foldl killPlayer game players' in
            length (nub . map (view $ role . allegiance) . filterAlive $ game' ^. players) > 1
            ==> not . isGameOver $ run_ checkGameOver game'

prop_startGameStartsWithSunsetStage :: Property
prop_startGameStartsWithSunsetStage =
    forAll arbitraryPlayerSet $ \players ->
    isSunset (fst . fromRight . runExcept . runWriterT $ startGame "" players)

prop_startGameUsesGivenPlayers :: Property
prop_startGameUsesGivenPlayers =
    forAll arbitraryPlayerSet $ \players' ->
    (fst . fromRight . runExcept . runWriterT $ startGame "" players') ^. players == players'

prop_startGameErrorsUnlessUniquePlayerNames :: Game -> Property
prop_startGameErrorsUnlessUniquePlayerNames game =
    forAll (elements players') $ \player ->
    isLeft (runExcept . runWriterT $ startGame "" (player:players'))
    where
        players' = game ^. players

prop_startGameErrorsWhenLessThan7Players :: [Player] -> Property
prop_startGameErrorsWhenLessThan7Players players =
    length players < 7
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan24Players :: Property
prop_startGameErrorsWhenMoreThan24Players =
    forAll (resize 30 $ listOf arbitrary) $ \players ->
        length players > 24
        ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan1Defender :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1Defender players =
    length (filterDefenders players) > 1
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan1Scapegoat :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1Scapegoat players =
    length (filterScapegoats players) > 1
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan1Seer :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1Seer players =
    length (filterSeers players) > 1
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan1VillagerVillager :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1VillagerVillager players =
    length (filterVillagerVillagers players) > 1
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan1Witch :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1Witch players =
    length (filterWitches players) > 1
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan1WolfHound :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1WolfHound players =
    length (filterWolfHounds players) > 1
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_createPlayersUsesGivenPlayerNames :: [Text] -> [Role] -> Property
prop_createPlayersUsesGivenPlayerNames playerNames extraRoles = monadicIO $ do
    players <- createPlayers playerNames extraRoles

    return $ playerNames == map (view name) players

prop_createPlayersUsesGivenRoles :: [Text] -> [Role] -> Property
prop_createPlayersUsesGivenRoles playerNames extraRoles = monadicIO $ do
    players <- createPlayers playerNames extraRoles

    return $ extraRoles `isSubsequenceOf` map (view role) players

prop_createPlayersCreatesAlivePlayers :: [Text] -> [Role] -> Property
prop_createPlayersCreatesAlivePlayers playerNames extraRoles = monadicIO $ do
    players <- createPlayers playerNames extraRoles

    return $ all isAlive players

prop_randomiseRolesReturnsNRoles :: [Role] -> Int -> Property
prop_randomiseRolesReturnsNRoles extraRoles n = monadicIO $ do
    roles <- randomiseRoles extraRoles n

    return $ length roles == n

prop_randomiseRolesUsesGivenRoles :: [Role] -> Int -> Property
prop_randomiseRolesUsesGivenRoles extraRoles n = monadicIO $ do
    roles <- randomiseRoles extraRoles n

    return $ extraRoles `isSubsequenceOf` roles

prop_randomiseRolesProportionsAllegiances :: [Role] -> Int -> Property
prop_randomiseRolesProportionsAllegiances extraRoles n = monadicIO $ do
    roles <- randomiseRoles extraRoles n

    let werewolvesCount = length . elemIndices Role.Werewolves $ map (view allegiance) roles

    return $ werewolvesCount == n `quot` 6 + 1
