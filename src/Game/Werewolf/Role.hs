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
    Role(..), name, allegiance, description, advice,

    -- ** Instances
    allRoles, seerRole, villagerRole, werewolfRole,

    -- * Allegiance
    Allegiance(..),
) where

import Control.Lens

import Data.Aeson
#if !MIN_VERSION_aeson(0,10,0)
import Data.Aeson.Types
#endif
import Data.Text as T

import GHC.Generics

import Prelude hiding (all)

data Role = Role
    { _name        :: Text
    , _allegiance  :: Allegiance
    , _description :: Text
    , _advice      :: Text
    } deriving (Eq, Generic, Show)

instance FromJSON Role

instance ToJSON Role where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

data Allegiance = Villagers | Werewolves
    deriving (Eq, Generic, Show)

instance FromJSON Allegiance

instance ToJSON Allegiance where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

makeLenses ''Role

allRoles :: [Role]
allRoles = [seerRole, villagerRole, werewolfRole]

seerRole :: Role
seerRole = Role
    { _name         = "Seer"
    , _allegiance   = Villagers
    , _description  = T.unwords
        [ "A fortunate teller by other names, with the ability to see into fellow"
        , "townsfolk and determine their allegiance."
        ]
    , _advice       = T.unwords
        [ "Be extremely careful if you have discovered a Werewolf."
        , "It may be worth the pain of revealing yourself in order to identify the player,"
        , "but avoid doing this too early."
        ]
    }

villagerRole :: Role
villagerRole = Role
    { _name         = "Villager"
    , _allegiance   = Villagers
    , _description  = "An ordinary townsfolk humbly living in Millers Hollow."
    , _advice       =
        "Bluffing can be a good technique, but you had better be convincing about what you say."
    }

werewolfRole :: Role
werewolfRole = Role
    { _name         = "Werewolf"
    , _allegiance   = Werewolves
    , _description  = "A shapeshifting human that, at night, hunts the residents of Millers Hollow."
    , _advice       =
        "Voting against your partner can be a good way to deflect suspicion from yourself."
    }
