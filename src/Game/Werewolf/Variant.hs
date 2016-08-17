{-|
Module      : Game.Werewolf.Variant
Description : Simplistic variant data structure with lenses and instances.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Variants alter how a game plays out. Either by changing the messages returned, or by changing the
game logic.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Werewolf.Variant (
    -- * Variant
    Variant,
    tag, name, description,

    -- ** Instances
    allVariants,

    standardVariant, noRoleKnowledgeVariant, noRoleKnowledgeOrRevealVariant, noRoleRevealVariant,
    spitefulVillageVariant,

    -- ** Traversals
    standard, noRoleKnowledge, noRoleKnowledgeOrReveal, noRoleReveal, spitefulVillage,
) where

import Control.Lens

import Data.String.Humanise
import Data.String.Interpolate.Extra
import Data.Text                     as T

import Game.Werewolf.Role hiding (description, name, tag)

-- | Variant definitions require only a few pieces of information.
data Variant = Variant
    { _tag         :: Text
    , _name        :: Text
    , _description :: Text
    } deriving (Eq, Read, Show)

makeLenses ''Variant

instance Humanise Variant where
    humanise = view name

-- | A list containing all the variants defined in this file.
allVariants :: [Variant]
allVariants =
    [ standardVariant
    , noRoleKnowledgeVariant
    , noRoleKnowledgeOrRevealVariant
    , noRoleRevealVariant
    , spitefulVillageVariant
    ]

standardVariant :: Variant
standardVariant = Variant
    { _tag          = "standard"
    , _name         = T.strip [iFile|variant/standard/name.txt|]
    , _description  = T.strip [iFile|variant/standard/description.txt|]
    }

noRoleKnowledgeVariant :: Variant
noRoleKnowledgeVariant = Variant
    { _tag          = "no-role-knowledge"
    , _name         = T.strip [iFile|variant/no-role-knowledge/name.txt|]
    , _description  = T.strip [iFile|variant/no-role-knowledge/description.txt|]
    }

noRoleKnowledgeOrRevealVariant :: Variant
noRoleKnowledgeOrRevealVariant = Variant
    { _tag          = "no-role-knowledge-or-reveal"
    , _name         = T.strip [iFile|variant/no-role-knowledge-or-reveal/name.txt|]
    , _description  = T.strip [iFile|variant/no-role-knowledge-or-reveal/description.txt|]
    }

noRoleRevealVariant :: Variant
noRoleRevealVariant = Variant
    { _tag          = "no-role-reveal"
    , _name         = T.strip [iFile|variant/no-role-reveal/name.txt|]
    , _description  = T.strip [iFile|variant/no-role-reveal/description.txt|]
    }

spitefulVillageVariant :: Variant
spitefulVillageVariant = Variant
    { _tag          = "spiteful-village"
    , _name         = T.strip [iFile|variant/spiteful-village/name.txt|]
    , _description  = T.strip [iFile|variant/spiteful-village/description.txt|]
    }

-- | The traversal of 'standard' 'Variant's.
--
-- @
-- 'standard' = 'only' 'standardVariant'
-- @
standard :: Traversal' Variant ()
standard = only standardVariant

-- | The traversal of 'noRoleKnowledge' 'Variant's.
--
-- @
-- 'noRoleKnowledge' = 'only' 'noRoleKnowledgeVariant'
-- @
noRoleKnowledge :: Traversal' Variant ()
noRoleKnowledge = only noRoleKnowledgeVariant

-- | The traversal of 'noRoleKnowledgeOrReveal' 'Variant's.
--
-- @
-- 'noRoleKnowledgeOrReveal' = 'only' 'noRoleKnowledgeOrRevealVariant'
-- @
noRoleKnowledgeOrReveal :: Traversal' Variant ()
noRoleKnowledgeOrReveal = only noRoleKnowledgeOrRevealVariant

-- | The traversal of 'noRoleReveal' 'Variant's.
--
-- @
-- 'noRoleReveal' = 'only' 'noRoleRevealVariant'
-- @
noRoleReveal :: Traversal' Variant ()
noRoleReveal = only noRoleRevealVariant

-- | The traversal of 'spitefulVillage' 'Variant's.
--
-- @
-- 'spitefulVillage' = 'only' 'spitefulVillageVariant'
-- @
spitefulVillage :: Traversal' Variant ()
spitefulVillage = only spitefulVillageVariant
