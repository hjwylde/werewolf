{-|
Module      : Game.Werewolf.Test.Game
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Game (
    prop_newGameStartsWithSeersTurn, prop_newGameUsesGivenPlayers, check_newSeersTurn,
    check_newVillagersTurn, check_newWerewolvesTurn,
) where

import Control.Lens

import Data.Map as Map

import Game.Werewolf.Game
import Game.Werewolf.Player

import Test.HUnit

prop_newGameStartsWithSeersTurn :: [Player] -> Bool
prop_newGameStartsWithSeersTurn players = isSeersTurn $ newGame players

prop_newGameUsesGivenPlayers :: [Player] -> Bool
prop_newGameUsesGivenPlayers players_ = newGame players_ ^. players == players_

check_newSeersTurn :: Assertion
check_newSeersTurn = newSeersTurn @?= Seers Map.empty

check_newVillagersTurn :: Assertion
check_newVillagersTurn = newVillagersTurn @?= Villagers Map.empty

check_newWerewolvesTurn :: Assertion
check_newWerewolvesTurn = newWerewolvesTurn @?= Werewolves Map.empty
