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

    , testProperty "check scapegoat's turn advances to seer's turn"             prop_checkScapegoatsTurnAdvancesToSeersTurn
    , testProperty "check scapegoat's turn does nothing while scapegoat blamed" prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed
    ]

prop_checkStageSkipsScapegoatsTurnWhenNoScapegoat :: GameWithConflictingVote -> Bool
prop_checkStageSkipsScapegoatsTurnWhenNoScapegoat (GameWithConflictingVote game) =
    hasn't (stage . _ScapegoatsTurn) game'
    where
        scapegoatsName  = game ^?! players . scapegoats . name
        game'           = run_ (apply (quitCommand scapegoatsName) >> checkStage) game

prop_checkScapegoatsTurnAdvancesToSeersTurn :: GameWithAllowedVoters -> Bool
prop_checkScapegoatsTurnAdvancesToSeersTurn (GameWithAllowedVoters game) =
    has (stage . _SeersTurn) (run_ checkStage game)

prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed :: GameAtScapegoatsTurn -> Bool
prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed (GameAtScapegoatsTurn game) =
    has (stage . _ScapegoatsTurn) (run_ checkStage game)
