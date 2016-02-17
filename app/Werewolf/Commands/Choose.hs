{-|
Module      : Werewolf.Commands.Choose
Description : Options and handler for the choose subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the choose subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Commands.Choose (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import           Data.Text (Text)
import qualified Data.Text as T

import Game.Werewolf.Command
import Game.Werewolf.Engine   hiding (isWildChildsTurn)
import Game.Werewolf.Game
import Game.Werewolf.Response

data Options = Options
    { arg :: Text
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options arg) = do
    unlessM doesGameExist $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame

    let command = case game ^. stage of
            ScapegoatsTurn  -> choosePlayersCommand callerName (filter (/= T.empty) (T.splitOn "," arg))
            WildChildsTurn  -> choosePlayerCommand callerName arg
            WolfHoundsTurn  -> chooseAllegianceCommand callerName arg
            -- TODO (hjw): throw an error
            _               -> undefined

    case runExcept (runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeGame game >> exitWith success { messages = messages }
