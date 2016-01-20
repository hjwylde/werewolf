{-|
Module      : Werewolf.Commands.Interpret
Description : Options for the interpret subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options for the interpret subcommand.
-}

module Werewolf.Commands.Interpret (
    -- * Options
    Options(..),
) where

import Data.Text (Text)

-- | Options.
data Options = Options
    { args :: [Text]
    } deriving (Eq, Show)
