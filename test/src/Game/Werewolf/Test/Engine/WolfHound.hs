{-|
Module      : Game.Werewolf.Test.Engine.WolfHound
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.WolfHound (
    -- * Tests
    allWolfHoundEngineTests,
) where

import Control.Lens

import Game.Werewolf
import Game.Werewolf.Command.Global
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allWolfHoundEngineTests :: [TestTree]
allWolfHoundEngineTests =
    [ testProperty "check stage skips wolf-hound's turn when no wolf-hound"             prop_checkStageSkipsWolfHoundsTurnWhenNoWolfHound

    , testProperty "check wolf-hound's turn advances to seer's turn"                prop_checkWolfHoundsTurnAdvancesToSeersTurn
    , testProperty "check wolf-hound's turn advances when no wolf-hound"            prop_checkWolfHoundsTurnAdvancesWhenNoWolfHound
    , testProperty "check wolf-hound's turn does nothing unless allegiance chosen"  prop_checkWolfHoundsTurnDoesNothingUnlessAllegianceChosen
    ]

prop_checkStageSkipsWolfHoundsTurnWhenNoWolfHound :: GameWithProtect -> Bool
prop_checkStageSkipsWolfHoundsTurnWhenNoWolfHound (GameWithProtect game) =
    hasn't (stage . _WolfHoundsTurn) game'
    where
        wolfHoundsName  = game ^?! players . wolfHounds . name
        game'           = run_ (apply (quitCommand wolfHoundsName) >> checkStage) game

prop_checkWolfHoundsTurnAdvancesToSeersTurn :: GameAtWolfHoundsTurn -> Property
prop_checkWolfHoundsTurnAdvancesToSeersTurn (GameAtWolfHoundsTurn game) =
    forAll (arbitraryWolfHoundChooseCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command >> checkStage) game

        has (stage . _SeersTurn) game'

prop_checkWolfHoundsTurnAdvancesWhenNoWolfHound :: GameAtWolfHoundsTurn -> Bool
prop_checkWolfHoundsTurnAdvancesWhenNoWolfHound (GameAtWolfHoundsTurn game) = do
    let wolfHound   = game ^?! players . wolfHounds
    let command     = quitCommand $ wolfHound ^. name

    hasn't (stage . _WolfHoundsTurn) (run_ (apply command >> checkStage) game)

prop_checkWolfHoundsTurnDoesNothingUnlessAllegianceChosen :: GameAtWolfHoundsTurn -> Bool
prop_checkWolfHoundsTurnDoesNothingUnlessAllegianceChosen (GameAtWolfHoundsTurn game) =
    has (stage . _WolfHoundsTurn) (run_ checkStage game)
