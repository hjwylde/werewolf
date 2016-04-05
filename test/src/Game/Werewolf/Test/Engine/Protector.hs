{-|
Module      : Game.Werewolf.Test.Engine.Protector
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Protector (
    -- * Tests
    allProtectorEngineTests,
) where

import Control.Lens

import Game.Werewolf
import Game.Werewolf.Command.Global
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allProtectorEngineTests :: [TestTree]
allProtectorEngineTests =
    [ testProperty "check stage skips protector's turn when no protector" prop_checkStageSkipsProtectorsTurnWhenNoProtector

    , testProperty "check protector's turn advances to werewolves' turn"    prop_checkProtectorsTurnAdvancesToWerewolvesTurn
    , testProperty "check protector's turn advances when no protector"      prop_checkProtectorsTurnAdvancesWhenNoProtector
    , testProperty "check protector's turn does nothing unless protected"   prop_checkProtectorsTurnDoesNothingUnlessProtected
    ]

prop_checkStageSkipsProtectorsTurnWhenNoProtector :: GameWithRoleModel -> Bool
prop_checkStageSkipsProtectorsTurnWhenNoProtector (GameWithRoleModel game) =
    hasn't (stage . _ProtectorsTurn) game'
    where
        protectorsName  = game ^?! players . protectors . name
        game'           = run_ (apply (quitCommand protectorsName) >> checkStage) game

prop_checkProtectorsTurnAdvancesToWerewolvesTurn :: GameWithProtect -> Bool
prop_checkProtectorsTurnAdvancesToWerewolvesTurn (GameWithProtect game) =
    has (stage . _WerewolvesTurn) (run_ checkStage game)

prop_checkProtectorsTurnAdvancesWhenNoProtector :: GameAtProtectorsTurn -> Bool
prop_checkProtectorsTurnAdvancesWhenNoProtector (GameAtProtectorsTurn game) = do
    let protector   = game ^?! players . protectors
    let command     = quitCommand $ protector ^. name

    hasn't (stage . _ProtectorsTurn) (run_ (apply command >> checkStage) game)

prop_checkProtectorsTurnDoesNothingUnlessProtected :: GameAtProtectorsTurn -> Bool
prop_checkProtectorsTurnDoesNothingUnlessProtected (GameAtProtectorsTurn game) =
    has (stage . _ProtectorsTurn) (run_ checkStage game)
