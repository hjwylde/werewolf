{-|
Module      : Game.Werewolf.Role
Description : Role data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Role data structures.
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Werewolf.Role (
    -- * Role
    Role(..), name, description, advice,

    -- ** Instances
    allRoles, villager, werewolf,
) where

import Control.Lens

import Data.Aeson
#if !MIN_VERSION_aeson(0,10,0)
import Data.Aeson.Types
#endif
import Data.Text (Text)

import GHC.Generics

import Prelude hiding (all)

data Role = Role
    { _name        :: Text
    , _description :: Text
    , _advice      :: Text
    } deriving (Eq, Generic, Show)

instance FromJSON Role

instance ToJSON Role where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

makeLenses ''Role

allRoles :: [Role]
allRoles = [villager, werewolf]

villager :: Role
villager = Role "Villager" "An ordinary townsfolk humbly living in Millers Hollow." "Bluffing can be a good technique, but you had better be convincing about what you say."

werewolf :: Role
werewolf = Role "Werewolf" "A shapeshifting human that, at night, hunts the residents of Millers Hollow." "Voting against your partner can be a good way to deflect suspicion from yourself."
