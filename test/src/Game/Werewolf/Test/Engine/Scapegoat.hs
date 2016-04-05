{-|
Module      : Game.Werewolf.Test.Engine.Scapegoat
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Scapegoat (
    -- * Tests
    allScapegoatEngineTests,
) where

import Control.Lens

import Game.Werewolf
import Game.Werewolf.Command.Global
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allScapegoatEngineTests :: [TestTree]
allScapegoatEngineTests =
    [ testProperty "check stage skips scapegoat's turn when no scapegoat" prop_checkStageSkipsScapegoatsTurnWhenNoScapegoat

    , testProperty "check scapegoat's turn advances to wolf-hound's turn"                   prop_checkScapegoatsTurnAdvancesToWolfHoundsTurn
    , testProperty "check scapegoat's turn skips wolf-hound's turn when allegiance chosen"  prop_checkScapegoatsTurnSkipsWolfHoundsTurnWhenAllegianceChosen
    , testProperty "check scapegoat's turn does nothing while scapegoat blamed"             prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed
    ]

prop_checkStageSkipsScapegoatsTurnWhenNoScapegoat :: GameWithConflictingVote -> Bool
prop_checkStageSkipsScapegoatsTurnWhenNoScapegoat (GameWithConflictingVote game) =
    hasn't (stage . _ScapegoatsTurn) game'
    where
        scapegoatsName  = game ^?! players . scapegoats . name
        game'           = run_ (apply (quitCommand scapegoatsName) >> checkStage) game

prop_checkScapegoatsTurnAdvancesToWolfHoundsTurn :: GameWithAllowedVoters -> Bool
prop_checkScapegoatsTurnAdvancesToWolfHoundsTurn (GameWithAllowedVoters game) =
    has (stage . _WolfHoundsTurn) (run_ checkStage game)

prop_checkScapegoatsTurnSkipsWolfHoundsTurnWhenAllegianceChosen :: GameWithAllowedVoters -> Bool
prop_checkScapegoatsTurnSkipsWolfHoundsTurnWhenAllegianceChosen (GameWithAllowedVoters game) = do
    let game' = game & allegianceChosen .~ True

    hasn't (stage . _WolfHoundsTurn) (run_ checkStage game')

prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed :: GameAtScapegoatsTurn -> Bool
prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed (GameAtScapegoatsTurn game) =
    has (stage . _ScapegoatsTurn) (run_ checkStage game)
