{-|
Module      : Game.Werewolf.Player
Description : Player data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Player data structures.
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Werewolf.Player (
    -- * Player
    Player(..), name, role, state,
    newPlayer,

    -- ** Searches
    findByName, findByName_,

    -- ** Filters
    filterSeers, filterVillagers, filterWerewolves,

    -- ** Queries
    doesPlayerExist, isSeer, isVillager, isWerewolf, isAlive, isDead,

    -- * State
    State(..),

    -- ** Filters
    filterAlive, filterDead,
) where

import Control.Lens

import Data.Aeson
#if !MIN_VERSION_aeson(0,10,0)
import Data.Aeson.Types
#endif
import Data.List
import Data.Maybe
import Data.Text  (Text)

import Game.Werewolf.Role hiding (name, _name)
import GHC.Generics

data Player = Player
    { _name  :: Text
    , _role  :: Role
    , _state :: State
    } deriving (Eq, Generic, Show)

instance FromJSON Player

instance ToJSON Player where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

data State = Alive | Dead
    deriving (Eq, Generic, Show)

instance FromJSON State

instance ToJSON State where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

makeLenses ''Player

newPlayer :: Text -> Role -> Player
newPlayer name role = Player name role Alive

findByName :: Text -> [Player] -> Maybe Player
findByName name = find ((==) name . _name)

findByName_ :: Text -> [Player] -> Player
findByName_ name = fromJust . findByName name

filterSeers :: [Player] -> [Player]
filterSeers = filter isSeer

filterVillagers :: [Player] -> [Player]
filterVillagers = filter isVillager

filterWerewolves :: [Player] -> [Player]
filterWerewolves = filter isWerewolf

doesPlayerExist :: Text -> [Player] -> Bool
doesPlayerExist name = isJust . findByName name

isSeer :: Player -> Bool
isSeer player = player ^. role == seerRole

isVillager :: Player -> Bool
isVillager player = player ^. role == villagerRole

isWerewolf :: Player -> Bool
isWerewolf player = player ^. role == werewolfRole

isAlive :: Player -> Bool
isAlive player = player ^. state == Alive

isDead :: Player -> Bool
isDead player = player ^. state == Dead

filterAlive :: [Player] -> [Player]
filterAlive = filter isAlive

filterDead :: [Player] -> [Player]
filterDead = filter isDead
