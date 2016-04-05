{-|
Module      : Game.Werewolf.Test.Engine.Village
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Village (
    -- * Tests
    allVillageEngineTests,
) where

import Control.Lens hiding (isn't)

import Game.Werewolf
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allVillageEngineTests :: [TestTree]
allVillageEngineTests =
    [ testProperty "check stage skips village's turn when allowed voters empty"         prop_checkStageSkipsVillagesTurnWhenAllowedVotersEmpty

    , testProperty "check villages' turn advances to devoted servant's turn"            prop_checkVillagesTurnAdvancesToDevotedServantsTurn
    , testProperty "check villages' turn skips devoted servant's turn when conflicted"  prop_checkVillagesTurnSkipsDevotedServantsTurnWhenConflicted
    , testProperty "check villages' turn does nothing unless all voted"                 prop_checkVillagesTurnDoesNothingUnlessAllVoted
    ]

prop_checkStageSkipsVillagesTurnWhenAllowedVotersEmpty :: GameAtWitchsTurn -> Property
prop_checkStageSkipsVillagesTurnWhenAllowedVotersEmpty (GameAtWitchsTurn game) =
    forAll (arbitraryWitchPassCommand game') $ \(Blind passWitchsTurnCommand) -> do
        hasn't (stage . _VillagesTurn) (run_ (apply passWitchsTurnCommand >> checkStage) game')
    where
        game' = game & allowedVoters .~ []

prop_checkVillagesTurnAdvancesToDevotedServantsTurn :: GameWithMajorityVote -> Property
prop_checkVillagesTurnAdvancesToDevotedServantsTurn (GameWithMajorityVote game) =
    isn't angel target && isn't devotedServant target
    ==> has (stage . _DevotedServantsTurn) (run_ checkStage game)
    where
        target = head $ getVoteResult game

prop_checkVillagesTurnSkipsDevotedServantsTurnWhenConflicted :: GameWithConflictingVote -> Bool
prop_checkVillagesTurnSkipsDevotedServantsTurnWhenConflicted (GameWithConflictingVote game) =
    hasn't (stage . _DevotedServantsTurn) (run_ checkStage game)

prop_checkVillagesTurnDoesNothingUnlessAllVoted :: GameAtVillagesTurn -> Property
prop_checkVillagesTurnDoesNothingUnlessAllVoted (GameAtVillagesTurn game) =
    forAll (runArbitraryCommands n game) $ \game' ->
    has (stage . _VillagesTurn) (run_ checkStage game')
    where
        n = length (game ^. players) - 1
