{-|
Module      : Game.Werewolf.Test.Game
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Game.Werewolf.Test.Game (
    -- * Tests
    allGameTests,
) where

import Control.Lens

import qualified Data.Map   as Map
import           Data.Maybe

import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Test.Arbitrary ()

import Prelude hiding (round)

import Test.Tasty
import Test.Tasty.QuickCheck

allGameTests :: [TestTree]
allGameTests =
    [ testProperty "new game starts with sunset stage"      prop_newGameStartsWithSunsetStage
    , testProperty "new game starts on first round"         prop_newGameStartsOnFirstRound
    , testProperty "new game starts with events empty"      prop_newGameStartsWithEventsEmpty
    , testProperty "new game starts with passes empty"      prop_newGameStartsWithPassesEmpty
    , testProperty "new game starts with no heal"           prop_newGameStartsWithNoHeal
    , testProperty "new game starts with no heal used"      prop_newGameStartsWithNoHealUsed
    , testProperty "new game starts with no poison"         prop_newGameStartsWithNoPoison
    , testProperty "new game starts with no poison used"    prop_newGameStartsWithNoPoisonUsed
    , testProperty "new game starts with no prior protect"  prop_newGameStartsWithNoPriorProtect
    , testProperty "new game starts with no protect"        prop_newGameStartsWithNoProtect
    , testProperty "new game starts with no see"            prop_newGameStartsWithNoSee
    , testProperty "new game starts with votes empty"       prop_newGameStartsWithVotesEmpty
    , testProperty "new game uses given players"            prop_newGameUsesGivenPlayers
    ]

prop_newGameStartsWithSunsetStage :: [Player] -> Bool
prop_newGameStartsWithSunsetStage players = isSunset (newGame players)

prop_newGameStartsOnFirstRound :: [Player] -> Bool
prop_newGameStartsOnFirstRound players = isFirstRound $ newGame players

prop_newGameStartsWithEventsEmpty :: [Player] -> Bool
prop_newGameStartsWithEventsEmpty players = null $ newGame players ^. events

prop_newGameStartsWithPassesEmpty :: [Player] -> Bool
prop_newGameStartsWithPassesEmpty players = null $ newGame players ^. passes

prop_newGameStartsWithNoHeal :: [Player] -> Bool
prop_newGameStartsWithNoHeal players = not $ newGame players ^. heal

prop_newGameStartsWithNoHealUsed :: [Player] -> Bool
prop_newGameStartsWithNoHealUsed players = not $ newGame players ^. healUsed

prop_newGameStartsWithNoPoison :: [Player] -> Bool
prop_newGameStartsWithNoPoison players = isNothing $ newGame players ^. poison

prop_newGameStartsWithNoPoisonUsed :: [Player] -> Bool
prop_newGameStartsWithNoPoisonUsed players = not $ newGame players ^. poisonUsed

prop_newGameStartsWithNoPriorProtect :: [Player] -> Bool
prop_newGameStartsWithNoPriorProtect players = isNothing $ newGame players ^. priorProtect

prop_newGameStartsWithNoProtect :: [Player] -> Bool
prop_newGameStartsWithNoProtect players = isNothing $ newGame players ^. protect

prop_newGameStartsWithNoSee :: [Player] -> Bool
prop_newGameStartsWithNoSee players = isNothing $ newGame players ^. see

prop_newGameStartsWithVotesEmpty :: [Player] -> Bool
prop_newGameStartsWithVotesEmpty players = Map.null $ newGame players ^. votes

prop_newGameUsesGivenPlayers :: [Player] -> Bool
prop_newGameUsesGivenPlayers players' = newGame players' ^. players == players'
