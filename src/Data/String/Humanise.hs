{-|
Module      : Data.String.Humanise
Description : Humanise type class for pretty printing data structures.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Humanise type class for displaying data structures to a human.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.String.Humanise (
    -- * Humanise
    Humanise(..),
) where

import           Data.Text (Text)
import qualified Data.Text as T

class Humanise a where
    humanise :: a -> Text

instance Humanise Text where
    humanise = id

instance Humanise a => Humanise [a] where
    humanise []     = ""
    humanise [word] = humanise word
    humanise words  = T.unwords [T.intercalate ", " (map humanise $ init words), "and", humanise $ last words]
