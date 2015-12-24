{-|
Module      : Game.Werewolf.Test.Game
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Game (
    prop_newGameStartsWithWerewolvesTurn, check_newVillagersTurn, check_newWerewolvesTurn,
) where

import Control.Lens

import Data.Map as Map

import Game.Werewolf.Game
import Game.Werewolf.Player

import Test.HUnit

prop_newGameStartsWithWerewolvesTurn :: [Player] -> Bool
prop_newGameStartsWithWerewolvesTurn players = newGame players ^. turn == newWerewolvesTurn

check_newVillagersTurn :: Assertion
check_newVillagersTurn = newVillagersTurn @?= Villagers Map.empty

check_newWerewolvesTurn :: Assertion
check_newWerewolvesTurn = newWerewolvesTurn @?= Werewolves Map.empty
