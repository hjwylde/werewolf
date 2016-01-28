{-|
Module      : Game.Werewolf.Test.Game
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Game.Werewolf.Test.Game (
    prop_newGameStartsWithSunsetStage, prop_newGameStartsWithNoSee,
    prop_newGameStartsWithVotesEmpty, prop_newGameUsesGivenPlayers,
) where

import Control.Lens

import qualified Data.Map   as Map
import           Data.Maybe

import Game.Werewolf.Game
import Game.Werewolf.Player

prop_newGameStartsWithSunsetStage :: [Player] -> Bool
prop_newGameStartsWithSunsetStage players = isSunset (newGame players)

prop_newGameStartsWithNoSee :: [Player] -> Bool
prop_newGameStartsWithNoSee players = isNothing $ newGame players ^. see

prop_newGameStartsWithVotesEmpty :: [Player] -> Bool
prop_newGameStartsWithVotesEmpty players = Map.null $ newGame players ^. votes

prop_newGameUsesGivenPlayers :: [Player] -> Bool
prop_newGameUsesGivenPlayers players' = newGame players' ^. players == players'
