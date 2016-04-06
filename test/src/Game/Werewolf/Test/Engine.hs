{-|
Module      : Game.Werewolf.Test.Engine
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Engine (
    -- * Tests
    allEngineTests,
) where

import Control.Lens         hiding (elements, isn't)
import Control.Monad.Except
import Control.Monad.Writer

import Data.Either.Extra
import Data.List.Extra

import Game.Werewolf
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Engine.Angel
import Game.Werewolf.Test.Engine.Hunter
import Game.Werewolf.Test.Engine.Lynching
import Game.Werewolf.Test.Engine.Orphan
import Game.Werewolf.Test.Engine.Protector
import Game.Werewolf.Test.Engine.Scapegoat
import Game.Werewolf.Test.Engine.Seer
import Game.Werewolf.Test.Engine.Village
import Game.Werewolf.Test.Engine.Werewolf
import Game.Werewolf.Test.Engine.Witch
import Game.Werewolf.Test.Engine.WolfHound
import Game.Werewolf.Test.Util

import Prelude hiding (round)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allEngineTests :: [TestTree]
allEngineTests = concat
    [ allAngelEngineTests
    , allHunterEngineTests
    , allLynchingEngineTests
    , allOrphanEngineTests
    , allProtectorEngineTests
    , allScapegoatEngineTests
    , allSeerEngineTests
    , allVillageEngineTests
    , allWerewolfEngineTests
    , allWitchEngineTests
    , allWolfHoundEngineTests

    , allGameOverTests
    , allStartGameTests
    ]

allGameOverTests :: [TestTree]
allGameOverTests =
    [ testProperty "check game over advances stage when one allegiance alive"                   prop_checkGameOverAdvancesStageWhenOneAllegianceAlive
    , testProperty "check game over advances stage when after first round and angel dead"       prop_checkGameOverAdvancesStageWhenAfterFirstRoundAndAngelDead
    , testProperty "check game over does nothing when angel dead but aligned with villagers"    prop_checkGameOverDoesNothingWhenAngelDeadButAlignedWithVillagers
    , testProperty "check game over does nothing when game over"                                prop_checkGameOverDoesNothingWhenGameOver
    ]

allStartGameTests :: [TestTree]
allStartGameTests =
    [ testProperty "start game uses given players"                              prop_startGameUsesGivenPlayers
    , testProperty "start game errors unless unique player names"               prop_startGameErrorsUnlessUniquePlayerNames
    , testProperty "start game errors when less than 7 players"                 prop_startGameErrorsWhenLessThan7Players
    , testProperty "start game errors when more than 1 of a restricted role"    prop_startGameErrorsWhenMoreThan1OfARestrictedRole
    ]

prop_checkGameOverAdvancesStageWhenOneAllegianceAlive :: GameWithOneAllegianceAlive -> Property
prop_checkGameOverAdvancesStageWhenOneAllegianceAlive (GameWithOneAllegianceAlive game) =
    forAll (sublistOf $ game ^.. players . traverse . alive) $ \players' -> do
        let game' = foldr killPlayer game (players' ^.. names)

        has (stage . _GameOver) $ run_ checkGameOver game'

prop_checkGameOverDoesNothingWhenGameOver :: GameAtGameOver -> Property
prop_checkGameOverDoesNothingWhenGameOver (GameAtGameOver game) =
    run_ checkStage game === game

-- TODO (hjw): pending
--prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive :: GameWithDeadPlayers -> Property
--prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive (GameWithDeadPlayers game) =
--    length (nub . map (view $ role . allegiance) . filterAlive $ game ^. players) > 1
--    ==> not . is gameOver $ run_ checkGameOver game

prop_checkGameOverAdvancesStageWhenAfterFirstRoundAndAngelDead :: GameOnSecondRound -> Bool
prop_checkGameOverAdvancesStageWhenAfterFirstRoundAndAngelDead (GameOnSecondRound game) = do
    let angelsName  = game ^?! players . angels . name
    let game'       = killPlayer angelsName game

    has (stage . _GameOver) $ run_ checkGameOver game'

prop_checkGameOverDoesNothingWhenAngelDeadButAlignedWithVillagers :: GameOnSecondRound -> Bool
prop_checkGameOverDoesNothingWhenAngelDeadButAlignedWithVillagers (GameOnSecondRound game) = do
    let angelsName  = game ^?! players . angels . name
    let game'       = killPlayer angelsName game & players . traverse . filteredBy name angelsName . role . allegiance .~ Villagers

    hasn't (stage . _GameOver) $ run_ checkGameOver game'

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

prop_startGameErrorsWhenMoreThan1OfARestrictedRole :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1OfARestrictedRole players =
    any (\role' -> length (players ^.. traverse . filteredBy role role') > 1) restrictedRoles
    ==> isLeft . runExcept . runWriterT $ startGame "" players
