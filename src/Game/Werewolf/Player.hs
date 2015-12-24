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
    filterVillagers, filterWerewolves,

    -- ** Queries
    doesPlayerExist, isVillager, isWerewolf, isAlive, isDead,

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

filterRole :: Role -> [Player] -> [Player]
filterRole role = filter ((==) role . _role)

filterVillagers :: [Player] -> [Player]
filterVillagers = filterRole villager

filterWerewolves :: [Player] -> [Player]
filterWerewolves = filterRole werewolf

doesPlayerExist :: Text -> [Player] -> Bool
doesPlayerExist name = isJust . findByName name

isVillager :: Player -> Bool
isVillager player = player ^. role == villager

isWerewolf :: Player -> Bool
isWerewolf player = player ^. role == werewolf

isAlive :: Player -> Bool
isAlive player = player ^. state == Alive

isDead :: Player -> Bool
isDead player = player ^. state == Dead

filterState :: State -> [Player] -> [Player]
filterState state = filter ((==) state . _state)

filterAlive :: [Player] -> [Player]
filterAlive = filterState Alive

filterDead :: [Player] -> [Player]
filterDead = filterState Dead
