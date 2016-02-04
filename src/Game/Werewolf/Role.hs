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
    allRoles, defenderRole, scapegoatRole, seerRole, villagerRole, villagerVillagerRole,
    werewolfRole, witchRole,

    -- * Allegiance
    Allegiance(..),
) where

import Control.Lens

import           Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding (all)

data Role = Role
    { _name        :: Text
    , _allegiance  :: Allegiance
    , _description :: Text
    , _advice      :: Text
    } deriving (Eq, Read, Show)

allRoles :: [Role]
allRoles = [defenderRole, scapegoatRole, seerRole, villagerRole, villagerVillagerRole, werewolfRole, witchRole]

defenderRole :: Role
defenderRole = Role
    { _name         = "Defender"
    , _allegiance   = Villagers
    , _description  = T.unwords
        [ "A knight living in Miller's Hollow."
        , "The Defender has the ability to protect one player each night."
        ]
    , _advice       =
        "Be careful when you choose to protect someone, you cannot protect them 2 nights in a row."
    }

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

villagerVillagerRole :: Role
villagerVillagerRole = Role
    { _name         = "Villager-Villager"
    , _allegiance   = Villagers
    , _description  = "An honest townsperson humbly living in Millers Hollow."
    , _advice       = "You'll make friends quickly, but be wary about whom you trust."
    }

werewolfRole :: Role
werewolfRole = Role
    { _name         = "Werewolf"
    , _allegiance   = Werewolves
    , _description  =
        "A shapeshifting townsperson that, at night, hunts the residents of Millers Hollow."
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
        , "one poison potion which when used can kill a player."
        ]
    , _advice       = T.unwords
        [ "Each potion may only be used once per game,"
        , "but there are no restrictions on using both on one night."
        ]
    }

data Allegiance = Villagers | Werewolves
    deriving (Eq, Read, Show)

makeLenses ''Role
