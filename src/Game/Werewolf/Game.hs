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

    -- * Turn
    Turn(..), votes,
    newVillagersTurn, newWerewolvesTurn,

    -- ** Queries
    isVillagersTurn, isWerewolvesTurn, isGameOver,
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
    } deriving (Eq, Generic, Show)

instance FromJSON Game

instance ToJSON Game where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

data Turn
    = Villagers { _votes :: Map Text Text }
    | Werewolves { _votes :: Map Text Text }
    | NoOne
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
newGame = Game newWerewolvesTurn

newVillagersTurn :: Turn
newVillagersTurn = Villagers Map.empty

newWerewolvesTurn :: Turn
newWerewolvesTurn = Werewolves Map.empty

isVillagersTurn :: Game -> Bool
isVillagersTurn (Game (Villagers {}) _) = True
isVillagersTurn _                       = False

isWerewolvesTurn :: Game -> Bool
isWerewolvesTurn (Game (Werewolves {}) _)   = True
isWerewolvesTurn _                          = False

isGameOver :: Game -> Bool
isGameOver (Game turn _) = turn == NoOne
