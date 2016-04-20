{-|
Module      : Control.Lens.Extra
Description : Extra utility functions for working with lenses.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Extra utility functions for working with lenses.
-}

{-# LANGUAGE Rank2Types        #-}

module Control.Lens.Extra (
    -- * Folds
    is, isn't,

    -- * Traversals
    filteredBy,
) where

import Control.Lens hiding (isn't)

import           Data.Monoid

-- | The counter-part to 'isn't', but more general as it takes a 'Getting' instead.
--
--   @'is' = 'has'@
is :: Getting Any s a -> s -> Bool
is = has

-- | A re-write of 'Control.Lens.Prism.isn't' to be more general by taking a 'Getting' instead.
--
--   @'isn't' = 'hasn't'@
isn't :: Getting All s a -> s -> Bool
isn't = hasn't

-- | A companion to 'filtered' that, rather than using a predicate, filters on the given lens for
-- matches.
filteredBy :: Eq b => Lens' a b -> b -> Traversal' a a
filteredBy lens value = filtered ((value ==) . view lens)
