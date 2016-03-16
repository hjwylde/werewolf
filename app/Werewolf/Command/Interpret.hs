{-|
Module      : Werewolf.Command.Interpret
Description : Options for the interpret subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options for the interpret subcommand.
-}

module Werewolf.Command.Interpret (
    -- * Options
    Options(..),
) where

import Data.Text (Text)

data Options = Options
    { args :: [Text]
    } deriving (Eq, Show)
