{-|
Module      : Game.Werewolf.Test.Engine.Orphan
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Orphan (
    -- * Tests
    allOrphanEngineTests,
) where

import Control.Lens hiding (isn't)

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.DevotedServant
import Game.Werewolf.Command.Global
import Game.Werewolf.Command.Villager
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allOrphanEngineTests :: [TestTree]
allOrphanEngineTests =
    [ testProperty "check stage skips orphan's turn when no orphan" prop_checkStageSkipsOrphansTurnWhenNoOrphan

    , testProperty "check orphan's turn advances to protector's turn"   prop_checkOrphansTurnAdvancesToProtectorsTurn
    , testProperty "check orphan's turn advances when no orphan"        prop_checkOrphansTurnAdvancesWhenNoOrphan
    , testProperty "check orphan's turn does nothing unless chosen"     prop_checkOrphansTurnDoesNothingUnlessRoleModelChosen

    , testProperty "check sunset sets orphan's allegiance when role model dead" prop_checkSunsetSetsOrphansAllegianceWhenRoleModelDead
    ]

prop_checkStageSkipsOrphansTurnWhenNoOrphan :: GameWithSee -> Bool
prop_checkStageSkipsOrphansTurnWhenNoOrphan (GameWithSee game) =
    hasn't (stage . _OrphansTurn) game'
    where
        orphansName = game ^?! players . orphans . name
        game'       = run_ (apply (quitCommand orphansName) >> checkStage) game

prop_checkOrphansTurnAdvancesToProtectorsTurn :: GameAtOrphansTurn -> Property
prop_checkOrphansTurnAdvancesToProtectorsTurn (GameAtOrphansTurn game) =
    forAll (arbitraryOrphanChooseCommand game) $ \(Blind command) ->
    has (stage . _ProtectorsTurn) (run_ (apply command >> checkStage) game)

prop_checkOrphansTurnAdvancesWhenNoOrphan :: GameAtOrphansTurn -> Bool
prop_checkOrphansTurnAdvancesWhenNoOrphan (GameAtOrphansTurn game) = do
    let orphan  = game ^?! players . orphans
    let command = quitCommand $ orphan ^. name

    hasn't (stage . _OrphansTurn) (run_ (apply command >> checkStage) game)

prop_checkOrphansTurnDoesNothingUnlessRoleModelChosen :: GameAtOrphansTurn -> Bool
prop_checkOrphansTurnDoesNothingUnlessRoleModelChosen (GameAtOrphansTurn game) =
    has (stage . _OrphansTurn) (run_ checkStage game)

prop_checkSunsetSetsOrphansAllegianceWhenRoleModelDead :: GameWithRoleModelAtVillagesTurn -> Property
prop_checkSunsetSetsOrphansAllegianceWhenRoleModelDead (GameWithRoleModelAtVillagesTurn game) = do
    let game' = foldr (\player -> run_ (apply $ voteCommand (player ^. name) (roleModel' ^. name))) game (game ^. players)

    let devotedServantsName = game ^?! players . devotedServants . name
    let command             = passCommand devotedServantsName

    isn't angel roleModel' && isn't devotedServant roleModel' && isn't jester roleModel'
        ==> is werewolf $ run_ (checkStage >> apply command >> checkStage) game' ^?! players . orphans
    where
        roleModel' = game ^?! players . traverse . filteredBy name (fromJust $ game ^. roleModel)
