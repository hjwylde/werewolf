{-|
Module      : Game.Werewolf.Game
Description : Game and turn data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Game and turn data structures.
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Werewolf.Game (
    -- * Game
    Game(..), turn, players,
    newGame,

    -- ** Manipulations
    killPlayer,

    -- ** Queries
    isSeersTurn, isVillagersTurn, isWerewolvesTurn, isGameOver,

    -- * Turn
    Turn(..), sees, votes,
    turnRotation,
) where

import Control.Lens

import Data.Aeson
#if !MIN_VERSION_aeson(0,10,0)
import Data.Aeson.Types
#endif
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)

import Game.Werewolf.Player
import GHC.Generics

data Game = Game
    { _turn    :: Turn
    , _players :: [Player]
    , _sees    :: Map Text Text
    , _votes   :: Map Text Text
    } deriving (Eq, Generic, Show)

instance FromJSON Game

instance ToJSON Game where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

data Turn = Seers | Villagers | Werewolves | NoOne
    deriving (Eq, Generic, Show)

instance FromJSON Turn

instance ToJSON Turn where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

makeLenses ''Game

makeLenses ''Turn

newGame :: [Player] -> Game
newGame players = Game (head turnRotation) players Map.empty Map.empty

killPlayer :: Game -> Player -> Game
killPlayer game player = game & players %~ map (\player' -> if player' == player then player' & state .~ Dead else player')

isSeersTurn :: Game -> Bool
isSeersTurn game = game ^. turn == Seers

isVillagersTurn :: Game -> Bool
isVillagersTurn game = game ^. turn == Villagers

isWerewolvesTurn :: Game -> Bool
isWerewolvesTurn game = game ^. turn == Werewolves

isGameOver :: Game -> Bool
isGameOver game = game ^. turn == NoOne

turnRotation :: [Turn]
turnRotation = cycle [Seers, Werewolves, Villagers, NoOne]
