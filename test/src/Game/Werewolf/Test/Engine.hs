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
    prop_checkWerewolvesTurnDoesntSkipWitchsTurnWhenWitchDevoured,
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

prop_checkStageSkipsDefendersTurnWhenNoDefender :: Game -> Property
prop_checkStageSkipsDefendersTurnWhenNoDefender game =
    forAll (arbitrarySeeCommand game') $ \command ->
    not . isDefendersTurn $ run_ (apply command >> checkStage) game'
    where
        game' = foldl killPlayer game (filterDefenders $ game ^. players) & stage .~ SeersTurn

prop_checkStageSkipsSeersTurnWhenNoSeer :: Game -> Property
prop_checkStageSkipsSeersTurnWhenNoSeer game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    not . isSeersTurn $ run_ checkStage game''
    where
        game'   = foldl killPlayer game (filterSeers $ game ^. players) & stage .~ VillagesTurn
        n       = length $ game' ^. players

prop_checkStageSkipsWitchsTurnWhenNoWitch :: Game -> Property
prop_checkStageSkipsWitchsTurnWhenNoWitch game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    not . isWitchsTurn $ run_ checkStage game''
    where
        game'   = foldl killPlayer game (filterWitches $ game ^. players) & stage .~ WerewolvesTurn
        n       = length . filterWerewolves $ game' ^. players

prop_checkStageDoesNothingWhenGameOver :: Game -> Property
prop_checkStageDoesNothingWhenGameOver game =
    run_ checkStage game' === game'
    where
        game' = game & stage .~ GameOver

prop_checkDefendersTurnAdvancesToWerewolvesTurn :: Game -> Property
prop_checkDefendersTurnAdvancesToWerewolvesTurn game =
    forAll (arbitraryProtectCommand game') $ \command ->
    isWerewolvesTurn $ run_ (apply command >> checkStage) game'
    where
        game' = game & stage .~ DefendersTurn

prop_checkSeersTurnAdvancesToDefendersTurn :: Game -> Property
prop_checkSeersTurnAdvancesToDefendersTurn game =
    forAll (arbitraryCommand game') $ \command ->
    isDefendersTurn $ run_ (apply command >> checkStage) game'
    where
        game' = game & stage .~ SeersTurn

prop_checkSeersTurnResetsSee :: Game -> Property
prop_checkSeersTurnResetsSee game =
    forAll (arbitraryCommand game') $ \command ->
    isNothing $ run_ (apply command >> checkStage) game' ^. see
    where
        game' = game & stage .~ SeersTurn

prop_checkSeersTurnDoesNothingUnlessSeen :: Game -> Bool
prop_checkSeersTurnDoesNothingUnlessSeen game = isSeersTurn $ run_ checkStage game'
    where
        game' = game & stage .~ SeersTurn

prop_checkVillagesTurnAdvancesToSeersTurn :: Game -> Property
prop_checkVillagesTurnAdvancesToSeersTurn game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    not (null . filterAlive . filterSeers $ run_ checkStage game'' ^. players)
    ==> isSeersTurn $ run_ checkStage game''
    where
        game'   = game & stage .~ VillagesTurn
        n       = length $ game' ^. players

prop_checkVillagesTurnLynchesOnePlayerWhenConsensus :: Game -> Property
prop_checkVillagesTurnLynchesOnePlayerWhenConsensus game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') == 1
    ==> length (filterDead $ run_ checkStage game'' ^. players) == 1
    where
        game'   = game & stage .~ VillagesTurn
        n       = length $ game' ^. players

prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats :: Game -> Property
prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') > 1
    ==> length (filterDead $ run_ checkStage game'' ^. players) == length (filterDead $ game' ^. players)
    where
        game'   = foldl killPlayer game (filterScapegoats $ game ^. players) & stage .~ VillagesTurn
        n       = length $ game' ^. players

prop_checkVillagesTurnLynchesScapegoatWhenConflicted :: Game -> Property
prop_checkVillagesTurnLynchesScapegoatWhenConflicted game =
    forAll (runArbitraryCommands n game') $ \game'' -> and [
        length (getVoteResult game'') > 1,
        any isScapegoat $ game' ^. players
        ] ==> isScapegoat $ head (filterDead $ run_ checkStage game'' ^. players)
    where
        game'   = game & stage .~ VillagesTurn
        n       = length $ game' ^. players

prop_checkVillagesTurnResetsVotes :: Game -> Property
prop_checkVillagesTurnResetsVotes game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    Map.null $ run_ checkStage game'' ^. votes
    where
        game'   = game & stage .~ VillagesTurn
        n       = length $ game' ^. players

prop_checkVillagesTurnDoesNothingUnlessAllVoted :: Game -> Property
prop_checkVillagesTurnDoesNothingUnlessAllVoted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isVillagesTurn $ run_ checkStage game''
    where
        game'   = game & stage .~ VillagesTurn
        n       = length (game' ^. players) - 1

prop_checkWerewolvesTurnAdvancesToWitchsTurn :: Game -> Property
prop_checkWerewolvesTurnAdvancesToWitchsTurn game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') == 1
    ==> let target = head $ getVoteResult game''
        in not (isWitch target)
        ==> isWitchsTurn $ run_ checkStage game''
    where
        game'   = game & stage .~ WerewolvesTurn
        n       = length . filterWerewolves $ game' ^. players

prop_checkWerewolvesTurnDoesntSkipWitchsTurnWhenWitchDevoured :: Game -> Property
prop_checkWerewolvesTurnDoesntSkipWitchsTurnWhenWitchDevoured game =
    forAll (arbitraryWitch game) $ \witch ->
    let devourVoteCommands = map (\werewolf -> devourVoteCommand (werewolf ^. name) (witch ^. name)) (filterWerewolves $ game ^. players)
        game'' = foldl (flip $ run_ . apply) game' devourVoteCommands
    in isWitchsTurn $ run_ checkStage game''
    where
        game' = game & stage .~ WerewolvesTurn

prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned :: Game -> Bool
prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned game =
    not . isWitchsTurn $ run_ checkStage game'
    where
        game' = game & stage .~ WerewolvesTurn & healUsed .~ True & poisonUsed .~ True

prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus :: Game -> Property
prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') == 1
    ==> length (filterDead . filter (not . isWitch) $ run_ checkStage game'' ^. players) == 1
    where
        game'   = foldl killPlayer game (filterWitches $ game ^. players) & stage .~ WerewolvesTurn
        n       = length . filterWerewolves $ game' ^. players

prop_checkWerewolvesTurnKillsNoOneWhenConflicted :: Gen Bool
prop_checkWerewolvesTurnKillsNoOneWhenConflicted = do
    game <- suchThat arbitraryGameWithDevourVotes $ \game -> length (getVoteResult game) > 1

    return . isNothing . getDevourEvent $ run_ checkStage game

prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended :: Gen Bool
prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended = do
    game        <- suchThat arbitraryGameWithDevourVotes $ \game -> length (getVoteResult game) == 1
    let game'   = game & protect .~ Just (head (getVoteResult game) ^. name)

    return . isNothing . getDevourEvent $ run_ checkStage game'

prop_checkWerewolvesTurnResetsProtect :: Gen Bool
prop_checkWerewolvesTurnResetsProtect = do
    game <- arbitraryGameWithProtectAndDevourVotes

    return . isNothing $ run_ checkStage game ^. protect

prop_checkWerewolvesTurnResetsVotes :: Game -> Property
prop_checkWerewolvesTurnResetsVotes game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    Map.null $ run_ checkStage game'' ^. votes
    where
        game'   = game & stage .~ WerewolvesTurn
        n       = length . filterWerewolves $ game' ^. players

prop_checkWerewolvesTurnDoesNothingUnlessAllVoted :: Game -> Property
prop_checkWerewolvesTurnDoesNothingUnlessAllVoted game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    isWerewolvesTurn $ run_ checkStage game''
    where
        game'   = game & stage .~ WerewolvesTurn
        n       = length (filterWerewolves $ game' ^. players) - 1

prop_checkWitchsTurnAdvancesToVillagesTurn :: Game -> Property
prop_checkWitchsTurnAdvancesToVillagesTurn game =
    forAll (arbitraryPassCommand game') $ \command ->
    isVillagesTurn $ run_ (apply command >> checkStage) game'
    where
        game' = game & stage .~ WitchsTurn

prop_checkWitchsTurnHealsDevoureeWhenHealed :: Game -> Property
prop_checkWitchsTurnHealsDevoureeWhenHealed game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') == 1
    ==> let target = head $ getVoteResult game''
        in not (isWitch target)
        ==> let game''' = run_ checkStage game''
            in forAll (arbitraryHealCommand game''') $ \command ->
            forAll (arbitraryPassCommand game''') $ \passCommand ->
            null . filterDead $ run_ checkStage
                (run_ (apply passCommand) $
                    run_ (apply command) game'''
                    ) ^. players
    where
        game'   = game & stage .~ WerewolvesTurn
        n       = length . filterWerewolves $ game' ^. players

prop_checkWitchsTurnKillsOnePlayerWhenPoisoned :: Game -> Property
prop_checkWitchsTurnKillsOnePlayerWhenPoisoned game =
    forAll (arbitraryPoisonCommand game') $ \command ->
    forAll (arbitraryPassCommand game') $ \passCommand ->
    length (filterDead $ run_ (apply command >> apply passCommand >> checkStage) game' ^. players) == 1
    where
        game' = game & stage .~ WitchsTurn

prop_checkWitchsTurnDoesNothingWhenPassed :: Game -> Property
prop_checkWitchsTurnDoesNothingWhenPassed game =
    forAll (arbitraryPassCommand game') $ \command ->
    null . filterDead $ run_ (apply command >> checkStage) game' ^. players
    where
        game' = game & stage .~ WitchsTurn

prop_checkWitchsTurnResetsHeal :: Game -> Property
prop_checkWitchsTurnResetsHeal game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') == 1
    ==> let target = head $ getVoteResult game''
        in not (isWitch target)
        ==> let game''' = run_ checkStage game''
            in forAll (arbitraryHealCommand game''') $ \command ->
            forAll (arbitraryPassCommand game''') $ \passCommand ->
            not $ run_ checkStage
                (run_ (apply passCommand) $
                    run_ (apply command) game'''
                    ) ^. heal
    where
        game'   = game & stage .~ WerewolvesTurn
        n       = length . filterWerewolves $ game' ^. players


prop_checkWitchsTurnResetsPoison :: Game -> Property
prop_checkWitchsTurnResetsPoison game =
    forAll (arbitraryPoisonCommand game') $ \command ->
    isNothing $ run_ (apply command >> checkStage) game' ^. poison
    where
        game' = game & stage .~ WitchsTurn

prop_checkGameOverAdvancesStage :: Game -> Property
prop_checkGameOverAdvancesStage game =
    forAll (sublistOf $ game ^. players) $ \players' ->
    let game' = foldl killPlayer game players' in
        length (nub . map (view $ role . allegiance) . filterAlive $ game' ^. players) <= 1
        ==> isGameOver $ run_ checkGameOver game'

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
    forAll (elements players') $ \player -> isLeft (runExcept . runWriterT $ startGame "" (player:players'))
    where
        players' = game ^. players

prop_startGameErrorsWhenLessThan7Players :: [Player] -> Property
prop_startGameErrorsWhenLessThan7Players players =
    length players < 7
    ==> isLeft (runExcept . runWriterT $ startGame "" players)

prop_startGameErrorsWhenMoreThan24Players :: Property
prop_startGameErrorsWhenMoreThan24Players =
    forAll (resize 30 $ listOf arbitrary) $ \players ->
        length players > 24
        ==> isLeft (runExcept . runWriterT $ startGame "" players)

prop_startGameErrorsWhenMoreThan1Defender :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1Defender players =
    length (filterDefenders players) > 1
    ==> isLeft (runExcept . runWriterT $ startGame "" players)

prop_startGameErrorsWhenMoreThan1Scapegoat :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1Scapegoat players =
    length (filterScapegoats players) > 1
    ==> isLeft (runExcept . runWriterT $ startGame "" players)

prop_startGameErrorsWhenMoreThan1Seer :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1Seer players =
    length (filterSeers players) > 1
    ==> isLeft (runExcept . runWriterT $ startGame "" players)

prop_startGameErrorsWhenMoreThan1VillagerVillager :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1VillagerVillager players =
    length (filterVillagerVillagers players) > 1
    ==> isLeft (runExcept . runWriterT $ startGame "" players)

prop_startGameErrorsWhenMoreThan1Witch :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1Witch players =
    length (filterWitches players) > 1
    ==> isLeft (runExcept . runWriterT $ startGame "" players)

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

    return $ all ((Alive ==) . view state) players

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
