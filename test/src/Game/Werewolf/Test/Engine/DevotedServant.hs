{-|
Module      : Game.Werewolf.Test.Engine.DevotedServant
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.DevotedServant (
    -- * Tests
    allDevotedServantEngineTests,
) where

import Control.Lens hiding (isn't)

import Game.Werewolf
import Game.Werewolf.Command.Global
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allDevotedServantEngineTests :: [TestTree]
allDevotedServantEngineTests =
    [ testProperty "check stage skips devoted servant's turn when no devoted servant" prop_checkStageSkipsDevotedServantsTurnWhenNoDevotedServant

    , testProperty "check devoted servant's turn advances to wolf-hound's turn"             prop_checkDevotedServantsTurnAdvancesToWolfHoundsTurn
    , testProperty "check devoted servant's turn advances when no devoted servant"          prop_checkDevotedServantsTurnAdvancesWhenNoDevotedServant
    , testProperty "check devoted servant's turn does nothing unless revealed or passed"    prop_checkDevotedServantsTurnDoesNothingUnlessRevealedOrPassed
    ]

prop_checkStageSkipsDevotedServantsTurnWhenNoDevotedServant :: GameWithMajorityVote -> Bool
prop_checkStageSkipsDevotedServantsTurnWhenNoDevotedServant (GameWithMajorityVote game) =
    hasn't (stage . _DevotedServantsTurn) game'
    where
        devotedServantsName = game ^?! players . devotedServants . name
        game'               = run_ (apply (quitCommand devotedServantsName) >> checkStage) game

prop_checkDevotedServantsTurnAdvancesToWolfHoundsTurn :: GameAtDevotedServantsTurn -> Property
prop_checkDevotedServantsTurnAdvancesToWolfHoundsTurn (GameAtDevotedServantsTurn game) = do
    forAll (arbitraryCommand game) $ \(Blind command) ->
        isn't angel target && isn't wolfHound target
        ==> has (stage . _WolfHoundsTurn) (run_ (apply command >> checkStage) game)
    where
        target = head $ getVoteResult game

prop_checkDevotedServantsTurnAdvancesWhenNoDevotedServant :: GameAtDevotedServantsTurn -> Bool
prop_checkDevotedServantsTurnAdvancesWhenNoDevotedServant (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let command             = quitCommand devotedServantsName

    hasn't (stage . _DevotedServantsTurn) (run_ (apply command >> checkStage) game)

prop_checkDevotedServantsTurnDoesNothingUnlessRevealedOrPassed :: GameAtDevotedServantsTurn -> Bool
prop_checkDevotedServantsTurnDoesNothingUnlessRevealedOrPassed (GameAtDevotedServantsTurn game) =
    has (stage . _DevotedServantsTurn) (run_ checkStage game)
