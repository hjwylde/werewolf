{-|
Module      : Game.Werewolf.Test.Engine.Witch
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Witch (
    -- * Tests
    allWitchEngineTests,
) where

import Control.Lens

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.Global
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allWitchEngineTests :: [TestTree]
allWitchEngineTests =
    [ testProperty "check stage skips witch's turn when no witch" prop_checkStageSkipsWitchsTurnWhenNoWitch

    , testProperty "check witch's turn advances to villages' turn"              prop_checkWitchsTurnAdvancesToVillagesTurn
    , testProperty "check witch's turn advances when no witch"                  prop_checkWitchsTurnAdvancesWhenNoWitch
    , testProperty "check witch's turn heals devouree when healed"              prop_checkWitchsTurnHealsDevoureeWhenHealed
    , testProperty "check witch's turn kills one player when poisoned"          prop_checkWitchsTurnKillsOnePlayerWhenPoisoned
    , testProperty "check witch's turn does nothing when passed"                prop_checkWitchsTurnDoesNothingWhenPassed
    , testProperty "check witch's turn does nothing unless actioned or passed"  prop_checkWitchsTurnDoesNothingUnlessActionedOrPassed
    , testProperty "check witch's turn resets heal"                             prop_checkWitchsTurnResetsHeal
    , testProperty "check witch's turn resets poison"                           prop_checkWitchsTurnResetsPoison
    , testProperty "check witch's turn clears passed"                           prop_checkWitchsTurnClearsPassed
    ]

prop_checkStageSkipsWitchsTurnWhenNoWitch :: GameWithDevourVotes -> Property
prop_checkStageSkipsWitchsTurnWhenNoWitch (GameWithDevourVotes game) =
    null (run_ checkStage game' ^.. players . fallenAngels . dead)
    ==> hasn't (stage . _WitchsTurn) game'
    where
        witchsName  = game ^?! players . witches . name
        game'       = run_ (apply (quitCommand witchsName) >> checkStage) game

prop_checkWitchsTurnAdvancesToVillagesTurn :: GameAtWitchsTurn -> Property
prop_checkWitchsTurnAdvancesToVillagesTurn (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    has (stage . _VillagesTurn) (run_ (apply command >> checkStage) game)

prop_checkWitchsTurnAdvancesWhenNoWitch :: GameAtWitchsTurn -> Bool
prop_checkWitchsTurnAdvancesWhenNoWitch (GameAtWitchsTurn game) = do
    let witch   = game ^?! players . witches
    let command = quitCommand $ witch ^. name

    hasn't (stage . _WitchsTurn) (run_ (apply command >> checkStage) game)

prop_checkWitchsTurnHealsDevoureeWhenHealed :: GameWithHeal -> Property
prop_checkWitchsTurnHealsDevoureeWhenHealed (GameWithHeal game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    none (is dead) (run_ (apply command >> checkStage) game ^. players)

prop_checkWitchsTurnKillsOnePlayerWhenPoisoned :: GameWithPoison -> Property
prop_checkWitchsTurnKillsOnePlayerWhenPoisoned (GameWithPoison game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    length (run_ (apply command >> checkStage) game ^.. players . traverse . dead) == 1

prop_checkWitchsTurnDoesNothingWhenPassed :: GameAtWitchsTurn -> Property
prop_checkWitchsTurnDoesNothingWhenPassed (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command >> checkStage) game

        none (is dead) $ game' ^. players

prop_checkWitchsTurnDoesNothingUnlessActionedOrPassed :: GameAtWitchsTurn -> Bool
prop_checkWitchsTurnDoesNothingUnlessActionedOrPassed (GameAtWitchsTurn game) = do
    let game' = run_ checkStage game

    has (stage . _WitchsTurn) game'

prop_checkWitchsTurnResetsHeal :: GameWithHeal -> Property
prop_checkWitchsTurnResetsHeal (GameWithHeal game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command >> checkStage) game

        not $ game' ^. heal

prop_checkWitchsTurnResetsPoison :: GameWithPoison -> Property
prop_checkWitchsTurnResetsPoison (GameWithPoison game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command >> checkStage) game

        isNothing $ game' ^. poison

prop_checkWitchsTurnClearsPassed :: GameAtWitchsTurn -> Property
prop_checkWitchsTurnClearsPassed (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command >> checkStage) game

        not $ game' ^. passed
