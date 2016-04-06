{-|
Module      : Game.Werewolf.Test.Engine.Seer
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Seer (
    -- * Tests
    allSeerEngineTests,
) where

import Control.Lens

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.Global
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allSeerEngineTests :: [TestTree]
allSeerEngineTests =
    [ testProperty "check stage skips seer's turn when no seer" prop_checkStageSkipsSeersTurnWhenNoSeer

    , testProperty "check seer's turn advances to orphan's turn"        prop_checkSeersTurnAdvancesToOrphansTurn
    , testProperty "check seer's turn advances when no seer"            prop_checkSeersTurnAdvancesWhenNoSeer
    , testProperty "check seer's turn resets sees"                      prop_checkSeersTurnResetsSee
    , testProperty "check seer's turn does nothing unless seen"         prop_checkSeersTurnDoesNothingUnlessSeen
    ]

prop_checkStageSkipsSeersTurnWhenNoSeer :: GameWithMajorityVote -> Bool
prop_checkStageSkipsSeersTurnWhenNoSeer (GameWithMajorityVote game) =
    hasn't (stage . _SeersTurn) game'
    where
        seersName   = game ^?! players . seers . name
        game'       = run_ (apply (quitCommand seersName) >> checkStage) game

prop_checkSeersTurnAdvancesToOrphansTurn :: GameWithSee -> Bool
prop_checkSeersTurnAdvancesToOrphansTurn (GameWithSee game) =
    has (stage . _OrphansTurn) (run_ checkStage game)

prop_checkSeersTurnAdvancesWhenNoSeer :: GameAtSeersTurn -> Bool
prop_checkSeersTurnAdvancesWhenNoSeer (GameAtSeersTurn game) = do
    let seer    = game ^?! players . seers
    let command = quitCommand $ seer ^. name

    hasn't (stage . _SeersTurn) (run_ (apply command >> checkStage) game)

prop_checkSeersTurnResetsSee :: GameWithSee -> Bool
prop_checkSeersTurnResetsSee (GameWithSee game) =
    isNothing $ run_ checkStage game ^. see

prop_checkSeersTurnDoesNothingUnlessSeen :: GameAtSeersTurn -> Bool
prop_checkSeersTurnDoesNothingUnlessSeen (GameAtSeersTurn game) =
    has (stage . _SeersTurn) (run_ checkStage game)
