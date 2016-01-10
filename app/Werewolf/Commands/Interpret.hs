{-|
Module      : Werewolf.Commands.Interpret
Description : Options and handler for the interpret subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the interpret subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Commands.Interpret (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Except

import Data.Text (Text)

import qualified Werewolf.Commands.End   as End
import qualified Werewolf.Commands.Help  as Help
import qualified Werewolf.Commands.See   as See
import qualified Werewolf.Commands.Start as Start
import qualified Werewolf.Commands.Vote  as Vote

-- | Options.
data Options = Options
    { args :: [Text]
    } deriving (Eq, Show)

-- | Handle.
handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options args) = interpret callerName args

interpret :: MonadIO m => Text -> [Text] -> m ()
interpret callerName ["end"]                    = End.handle callerName
interpret callerName ["help"]                   = Help.handle callerName (Help.Options Nothing)
interpret callerName ("help":["description"])   = Help.handle callerName (Help.Options $ Just Help.Description)
interpret callerName ("help":["rules"])         = Help.handle callerName (Help.Options $ Just Help.Rules)
interpret callerName ("help":["roles"])         = Help.handle callerName (Help.Options $ Just Help.Roles)
interpret callerName ("see":[targetName])       = See.handle callerName (See.Options targetName)
interpret callerName ("start":playerNames)      = Start.handle callerName (Start.Options playerNames)
interpret callerName ("vote":[targetName])      = Vote.handle callerName (Vote.Options targetName)
interpret callerName _                          = Help.handle callerName (Help.Options $ Just Help.Commands)
