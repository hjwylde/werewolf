{-|
Module      : Game.Werewolf.Test.Game
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Game (
    prop_newGameStartsWithSeersTurn, prop_newGameStartsWithSeesEmpty,
    prop_newGameStartsWithVotesEmpty, prop_newGameUsesGivenPlayers,
) where

import Control.Lens

import Data.Map as Map

import Game.Werewolf.Game
import Game.Werewolf.Player

prop_newGameStartsWithSeersTurn :: [Player] -> Bool
prop_newGameStartsWithSeersTurn players = isSeersTurn $ newGame players

prop_newGameStartsWithSeesEmpty :: [Player] -> Bool
prop_newGameStartsWithSeesEmpty players = Map.null $ newGame players ^. sees

prop_newGameStartsWithVotesEmpty :: [Player] -> Bool
prop_newGameStartsWithVotesEmpty players = Map.null $ newGame players ^. votes

prop_newGameUsesGivenPlayers :: [Player] -> Bool
prop_newGameUsesGivenPlayers players' = newGame players' ^. players == players'
