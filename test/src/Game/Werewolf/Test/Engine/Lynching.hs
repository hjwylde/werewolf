{-|
Module      : Game.Werewolf.Test.Engine.Lynching
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Lynching (
    -- * Tests
    allLynchingEngineTests,
) where

import Control.Lens hiding (isn't)

import qualified Data.Map as Map

import Game.Werewolf
import Game.Werewolf.Command.Villager
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allLynchingEngineTests :: [TestTree]
allLynchingEngineTests =
    [ testProperty "check stage skips lynching when no votes" prop_checkStageSkipsLynchingWhenNoVotes

    , testProperty "check lynching lynches one player when consensus"                   prop_checkLynchingLynchesOnePlayerWhenConsensus
    , testProperty "check lynching lynches no one when target is jester"                prop_checkLynchingLynchesNoOneWhenTargetIsJester
    , testProperty "check lynching lynches scapegoat when conflicted"                   prop_checkLynchingLynchesScapegoatWhenConflicted
    , testProperty "check lynching lynches no one when conflicted and no scapegoats"    prop_checkLynchingLynchesNoOneWhenConflictedAndNoScapegoats
    , testProperty "check lynching resets votes"                                        prop_checkLynchingResetsVotes
    , testProperty "check lynching sets allowed voters"                                 prop_checkLynchingSetsAllowedVoters
    ]

prop_checkStageSkipsLynchingWhenNoVotes :: Game -> Bool
prop_checkStageSkipsLynchingWhenNoVotes _ = undefined

prop_checkLynchingLynchesOnePlayerWhenConsensus :: GameWithPassAtDevotedServantsTurn -> Property
prop_checkLynchingLynchesOnePlayerWhenConsensus (GameWithPassAtDevotedServantsTurn game) =
    isn't jester target
    ==> length (run_ checkStage game ^.. players . traverse . dead) == 1
    where
        target = head $ getVoteResult game

prop_checkLynchingLynchesNoOneWhenTargetIsJester :: GameAtVillagesTurn -> Bool
prop_checkLynchingLynchesNoOneWhenTargetIsJester (GameAtVillagesTurn game) = do
    let game' = foldr (\player -> run_ (apply $ voteCommand (player ^. name) (jester ^. name))) game (game ^. players)

    none (is dead) (run_ checkStage game' ^. players)
    where
        jester = game ^?! players . jesters

prop_checkLynchingLynchesScapegoatWhenConflicted :: GameAtScapegoatsTurn -> Bool
prop_checkLynchingLynchesScapegoatWhenConflicted (GameAtScapegoatsTurn game) =
    is dead $ run_ checkStage game ^?! players . scapegoats

-- TODO (hjw): tidy this test
prop_checkLynchingLynchesNoOneWhenConflictedAndNoScapegoats :: Game -> Property
prop_checkLynchingLynchesNoOneWhenConflictedAndNoScapegoats game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') > 1
    ==> run_ checkStage game'' ^. players == game' ^. players
    where
        scapegoatsName  = game ^?! players . scapegoats . name
        game'           = killPlayer scapegoatsName game & stage .~ VillagesTurn
        n               = length $ game' ^. players

prop_checkLynchingResetsVotes :: GameWithPassAtDevotedServantsTurn -> Property
prop_checkLynchingResetsVotes (GameWithPassAtDevotedServantsTurn game) =
    isn't devotedServant target
    ==> Map.null $ run_ checkStage game ^. votes
    where
        target = head $ getVoteResult game

prop_checkLynchingSetsAllowedVoters :: GameWithLynchVotes -> Property
prop_checkLynchingSetsAllowedVoters (GameWithLynchVotes game) =
    game' ^. allowedVoters === expectedAllowedVoters ^.. names
    where
        game' = run_ checkStage game
        expectedAllowedVoters
            | game' ^. jesterRevealed   = filter (isn't jester) $ game' ^. players
            | otherwise                 = game' ^.. players . traverse . alive
