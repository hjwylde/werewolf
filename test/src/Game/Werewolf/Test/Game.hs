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

import Game.Werewolf
import Game.Werewolf.Test.Arbitrary ()

import Prelude hiding (round)

import Test.Tasty
import Test.Tasty.QuickCheck

allGameTests :: [TestTree]
allGameTests =
    [ testProperty "new game starts at village's turn when angel in play"   prop_newGameStartsAtVillagesTurnWhenAngelInPlay
    , testProperty "new game starts at sunset when no angel in play"        prop_newGameStartsAtSunsetWhenNoAngelInPlay
    , testProperty "new game starts on first round"                         prop_newGameStartsOnFirstRound
    , testProperty "new game uses given players"                            prop_newGameUsesGivenPlayers
    , testProperty "new game starts with events empty"                      prop_newGameStartsWithEventsEmpty
    , testProperty "new game starts with boots empty"                       prop_newGameStartsWithBootsEmpty
    , testProperty "new game starts with allegiance chosen false"           prop_newGameStartsWithAllegianceChosenFalse
    , testProperty "new game starts with allowed voters full"               prop_newGameStartsWithAllowedVotersFull
    , testProperty "new game starts with heal false"                        prop_newGameStartsWithHealFalse
    , testProperty "new game starts with heal used false"                   prop_newGameStartsWithHealUsedFalse
    , testProperty "new game starts with hunter killed false"               prop_newGameStartsWithHunterKilledFalse
    , testProperty "new game starts with jester revealed false"             prop_newGameStartsWithJesterRevealedFalse
    , testProperty "new game starts with passed false"                      prop_newGameStartsWithPassedFalse
    , testProperty "new game starts with no poison"                         prop_newGameStartsWithNoPoison
    , testProperty "new game starts with poison used false"                 prop_newGameStartsWithPoisonUsedFalse
    , testProperty "new game starts with no prior protect"                  prop_newGameStartsWithNoPriorProtect
    , testProperty "new game starts with no protect"                        prop_newGameStartsWithNoProtect
    , testProperty "new game starts with no role model"                     prop_newGameStartsWithNoRoleModel
    , testProperty "new game starts with scapegoat blamed false"            prop_newGameStartsWithScapegoatBlamedFalse
    , testProperty "new game starts with no see"                            prop_newGameStartsWithNoSee
    , testProperty "new game starts with votes empty"                       prop_newGameStartsWithVotesEmpty
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

prop_newGameUsesGivenPlayers :: [Player] -> Bool
prop_newGameUsesGivenPlayers players' = newGame players' ^. players == players'

prop_newGameStartsWithEventsEmpty :: [Player] -> Bool
prop_newGameStartsWithEventsEmpty players = has (events . _Empty) (newGame players)

prop_newGameStartsWithBootsEmpty :: [Player] -> Bool
prop_newGameStartsWithBootsEmpty players = has (boots . _Empty) (newGame players)

prop_newGameStartsWithAllegianceChosenFalse :: [Player] -> Bool
prop_newGameStartsWithAllegianceChosenFalse players = not $ newGame players ^. allegianceChosen

prop_newGameStartsWithAllowedVotersFull :: [Player] -> Property
prop_newGameStartsWithAllowedVotersFull players = newGame players ^. allowedVoters === players ^.. names

prop_newGameStartsWithHealFalse :: [Player] -> Bool
prop_newGameStartsWithHealFalse players = not $ newGame players ^. heal

prop_newGameStartsWithHealUsedFalse :: [Player] -> Bool
prop_newGameStartsWithHealUsedFalse players = not $ newGame players ^. healUsed

prop_newGameStartsWithHunterKilledFalse :: [Player] -> Bool
prop_newGameStartsWithHunterKilledFalse players = not $ newGame players ^. hunterKilled

prop_newGameStartsWithJesterRevealedFalse :: [Player] -> Bool
prop_newGameStartsWithJesterRevealedFalse players = not $ newGame players ^. jesterRevealed

prop_newGameStartsWithPassedFalse :: [Player] -> Bool
prop_newGameStartsWithPassedFalse players = not $ newGame players ^. passed

prop_newGameStartsWithNoPoison :: [Player] -> Bool
prop_newGameStartsWithNoPoison players = has (poison . _Nothing) (newGame players)

prop_newGameStartsWithPoisonUsedFalse :: [Player] -> Bool
prop_newGameStartsWithPoisonUsedFalse players = not $ newGame players ^. poisonUsed

prop_newGameStartsWithNoPriorProtect :: [Player] -> Bool
prop_newGameStartsWithNoPriorProtect players = has (priorProtect . _Nothing) (newGame players)

prop_newGameStartsWithNoProtect :: [Player] -> Bool
prop_newGameStartsWithNoProtect players = has (protect . _Nothing) (newGame players)

prop_newGameStartsWithNoRoleModel :: [Player] -> Bool
prop_newGameStartsWithNoRoleModel players = has (roleModel . _Nothing) (newGame players)

prop_newGameStartsWithScapegoatBlamedFalse :: [Player] -> Bool
prop_newGameStartsWithScapegoatBlamedFalse players = not $ newGame players ^. scapegoatBlamed

prop_newGameStartsWithNoSee :: [Player] -> Bool
prop_newGameStartsWithNoSee players = has (see . _Nothing) (newGame players)

prop_newGameStartsWithVotesEmpty :: [Player] -> Bool
prop_newGameStartsWithVotesEmpty players = has (votes . _Empty) (newGame players)
