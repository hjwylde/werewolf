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
    allRoles, diurnalRoles, nocturnalRoles, scapegoatRole, seerRole, villagerRole, werewolfRole,
    witchRole,

    -- ** Queries
    findByName,

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
allRoles = [scapegoatRole, seerRole, villagerRole, werewolfRole, witchRole]

diurnalRoles :: [Role]
diurnalRoles = [scapegoatRole, villagerRole, witchRole]

nocturnalRoles :: [Role]
nocturnalRoles = allRoles \\ diurnalRoles

scapegoatRole :: Role
scapegoatRole = Role
    { _name         = "Scapegoat"
    , _allegiance   = Villagers
    , _description  = "That one person everyone loves to blame."
    , _advice       = "Cross your fingers that the votes don't end up tied."
    }

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

witchRole :: Role
witchRole = Role
    { _name         = "Witch"
    , _allegiance   = Villagers
    , _description  = T.unwords
        [ "A conniving townsperson."
        , "She knows how to make up 2 extremely powerful potions;"
        , "one healing potion which can revive the Werewolves' victim,"
        , "one poison potion which when used at night can kill a player."
        ]
    , _advice       = T.unwords
        [ "Each potion may be used once per game,"
        , "but there are no restrictions on how many you may use at night."
        ]
    }

findByName :: Text -> Maybe Role
findByName name = find ((name ==) . _name) allRoles

data Allegiance = Villagers | Werewolves
    deriving (Eq, Read, Show)

makeLenses ''Role
