{-|
Module      : Game.Werewolf.Test.Engine.Hunter
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Hunter (
    -- * Tests
    allHunterEngineTests,
) where

import Test.Tasty

allHunterEngineTests :: [TestTree]
allHunterEngineTests =
    -- TODO (hjw): pending
    [ --testProperty "check hunter's turn advances to wolf-hound's turn when hunter lynched"
    --, testProperty "check hunter's turn advances to village's turn when hunter devoured"
    --, testProperty "check hunter's turn does nothing until hunter retaliated"
    ]
