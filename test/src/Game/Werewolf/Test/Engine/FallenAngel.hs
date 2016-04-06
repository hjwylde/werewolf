{-|
Module      : Game.Werewolf.Test.Engine.Angel
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Engine.Angel (
    -- * Tests
    allAngelEngineTests,
) where

import Control.Lens

import Game.Werewolf
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Prelude hiding (round)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allAngelEngineTests :: [TestTree]
allAngelEngineTests =
    [ testProperty "check sunrise increments round"         prop_checkSunriseIncrementsRound
    , testProperty "check sunrise sets fallenAngel's allegiance"  prop_checkSunriseSetsAngelsAllegiance
    ]

prop_checkSunriseIncrementsRound :: GameAtSunrise -> Property
prop_checkSunriseIncrementsRound (GameAtSunrise game) =
    run_ checkStage game ^. round === game ^. round + 1

prop_checkSunriseSetsAngelsAllegiance :: GameAtSunrise -> Bool
prop_checkSunriseSetsAngelsAllegiance (GameAtSunrise game) = do
    let game' = run_ checkStage game

    is villager $ game' ^?! players . fallenAngels
