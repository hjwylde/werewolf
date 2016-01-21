{-|
Module      : Game.Werewolf.Role
Description : Role data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Role data structures.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Werewolf.Role (
    -- * Role
    Role(..), name, allegiance, description, advice,

    -- ** Instances
    allRoles, seerRole, villagerRole, werewolfRole,

    -- ** Queries
    findByName, findByName_,

    -- * Allegiance
    Allegiance(..),
) where

import Control.Lens

import           Data.List
import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

import Prelude hiding (all)

data Role = Role
    { _name        :: Text
    , _allegiance  :: Allegiance
    , _description :: Text
    , _advice      :: Text
    } deriving (Eq, Read, Show)

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
    , _description  = "An ordinary townsperson humbly living in Millers Hollow."
    , _advice       =
        "Bluffing can be a good technique, but you had better be convincing about what you say."
    }

werewolfRole :: Role
werewolfRole = Role
    { _name         = "Werewolf"
    , _allegiance   = Werewolves
    , _description  = "A shapeshifting townsperson that, at night, hunts the residents of Millers Hollow."
    , _advice       =
        "Voting against your partner can be a good way to deflect suspicion from yourself."
    }

findByName :: Text -> Maybe Role
findByName name = find ((==) name . _name) allRoles

findByName_ :: Text -> Role
findByName_ name = fromJust $ findByName name

data Allegiance = Villagers | Werewolves
    deriving (Eq, Read, Show)

makeLenses ''Role
