{-|
Module      : Data.String.Interpolate.Extra
Description : Extra utility functions for working with the interpolate Quasi Quoter.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Extra utility functions for working with the interpolate Quasi Quoter.
-}

module Data.String.Interpolate.Extra (
    -- * Quasi Quoters
    iFile,
) where

import Data.String.Interpolate.IsString

import Language.Haskell.TH.Quote

iFile :: QuasiQuoter
iFile = quoteFile i
