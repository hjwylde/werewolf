{-|
Module      : Werewolf.Commands.Start
Description : Options and handler for the start subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the start subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Commands.Start (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Except
import Control.Monad.Extra

import Data.Text (Text)

import Game.Werewolf.Engine
import Game.Werewolf.Response

-- | Options.
data Options = Options
    { argPlayers :: [Text]
    } deriving (Eq, Show)

-- | Handle.
handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options playerNames) = do
    whenM doesGameExist $ exitWith failure { messages = [privateMessage [callerName] "A game is already running."] }

    players <- createPlayers playerNames

    case runExcept (startGame callerName players) of
        Left errorMessages  -> exitWith failure { messages = errorMessages }
        Right game          -> writeGame game >> exitWith success { messages = newGameMessages players }
