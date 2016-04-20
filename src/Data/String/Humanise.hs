{-|
Module      : Data.String.Humanise
Description : Humanise type class for pretty printing data structures.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Humanise type class for displaying data structures to a human.
-}

module Data.String.Humanise (
    -- * Humanise
    Humanise(..),
) where

import Data.String

class Humanise a where
    humanise :: IsString b => a -> b
