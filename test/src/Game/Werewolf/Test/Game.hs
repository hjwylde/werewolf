{-|
Module      : Game.Werewolf.Test.Game
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Game (
    -- * Tests
    allGameTests,
) where

import Control.Lens

import Data.Maybe

import Game.Werewolf.Internal.Game
import Game.Werewolf.Internal.Player
import Game.Werewolf.Test.Arbitrary  ()

import Prelude hiding (round)

import Test.Tasty
import Test.Tasty.QuickCheck

allGameTests :: [TestTree]
allGameTests =
    [ testProperty "new game starts at village's turn when angel in play"   prop_newGameStartsAtVillagesTurnWhenAngelInPlay
    , testProperty "new game starts at sunset when no angel in play"        prop_newGameStartsAtSunsetWhenNoAngelInPlay
    , testProperty "new game starts on first round"                         prop_newGameStartsOnFirstRound
    , testProperty "new game starts with events empty"                      prop_newGameStartsWithEventsEmpty
    , testProperty "new game starts with passes empty"                      prop_newGameStartsWithPassesEmpty
    , testProperty "new game starts with allowed voters full"               prop_newGameStartsWithAllowedVotersFull
    , testProperty "new game starts with heal false"                        prop_newGameStartsWithHealFalse
    , testProperty "new game starts with heal used false"                   prop_newGameStartsWithHealUsedFalse
    , testProperty "new game starts with no poison"                         prop_newGameStartsWithNoPoison
    , testProperty "new game starts with poison used false"                 prop_newGameStartsWithPoisonUsedFalse
    , testProperty "new game starts with no prior protect"                  prop_newGameStartsWithNoPriorProtect
    , testProperty "new game starts with no protect"                        prop_newGameStartsWithNoProtect
    , testProperty "new game starts with scapegoat blamed false"            prop_newGameStartsWithScapegoatBlamedFalse
    , testProperty "new game starts with no see"                            prop_newGameStartsWithNoSee
    , testProperty "new game starts with village idiot revealed false"      prop_newGameStartsWithVillageIdiotRevealedFalse
    , testProperty "new game starts with votes empty"                       prop_newGameStartsWithVotesEmpty
    , testProperty "new game uses given players"                            prop_newGameUsesGivenPlayers
    ]

prop_newGameStartsAtVillagesTurnWhenAngelInPlay :: [Player] -> Property
prop_newGameStartsAtVillagesTurnWhenAngelInPlay players =
    has angels players
    ==> has (stage . _VillagesTurn) (newGame players)

prop_newGameStartsAtSunsetWhenNoAngelInPlay :: [Player] -> Property
prop_newGameStartsAtSunsetWhenNoAngelInPlay players =
    hasn't angels players
    ==> has (stage . _Sunset) (newGame players)

prop_newGameStartsOnFirstRound :: [Player] -> Bool
prop_newGameStartsOnFirstRound players = isFirstRound $ newGame players

prop_newGameStartsWithEventsEmpty :: [Player] -> Bool
prop_newGameStartsWithEventsEmpty players = has (events . _Empty) (newGame players)

prop_newGameStartsWithPassesEmpty :: [Player] -> Bool
prop_newGameStartsWithPassesEmpty players = has (passes . _Empty) (newGame players)

prop_newGameStartsWithAllowedVotersFull :: [Player] -> Property
prop_newGameStartsWithAllowedVotersFull players = newGame players ^. allowedVoters === players ^.. names

prop_newGameStartsWithHealFalse :: [Player] -> Bool
prop_newGameStartsWithHealFalse players = not $ newGame players ^. heal

prop_newGameStartsWithHealUsedFalse :: [Player] -> Bool
prop_newGameStartsWithHealUsedFalse players = not $ newGame players ^. healUsed

prop_newGameStartsWithNoPoison :: [Player] -> Bool
prop_newGameStartsWithNoPoison players = isNothing $ newGame players ^. poison

prop_newGameStartsWithPoisonUsedFalse :: [Player] -> Bool
prop_newGameStartsWithPoisonUsedFalse players = not $ newGame players ^. poisonUsed

prop_newGameStartsWithNoPriorProtect :: [Player] -> Bool
prop_newGameStartsWithNoPriorProtect players = isNothing $ newGame players ^. priorProtect

prop_newGameStartsWithNoProtect :: [Player] -> Bool
prop_newGameStartsWithNoProtect players = isNothing $ newGame players ^. protect

prop_newGameStartsWithScapegoatBlamedFalse :: [Player] -> Bool
prop_newGameStartsWithScapegoatBlamedFalse players = not $ newGame players ^. scapegoatBlamed

prop_newGameStartsWithNoSee :: [Player] -> Bool
prop_newGameStartsWithNoSee players = isNothing $ newGame players ^. see

prop_newGameStartsWithVillageIdiotRevealedFalse :: [Player] -> Bool
prop_newGameStartsWithVillageIdiotRevealedFalse players =
    not $ newGame players ^. villageIdiotRevealed

prop_newGameStartsWithVotesEmpty :: [Player] -> Bool
prop_newGameStartsWithVotesEmpty players = has (votes . _Empty) (newGame players)

prop_newGameUsesGivenPlayers :: [Player] -> Bool
prop_newGameUsesGivenPlayers players' = newGame players' ^. players == players'
