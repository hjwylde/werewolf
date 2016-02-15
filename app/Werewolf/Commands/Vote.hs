{-|
Module      : Werewolf.Commands.Vote
Description : Options and handler for the vote subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the vote subcommand.
-}

module Werewolf.Commands.Vote (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import Data.Text (Text)

import Game.Werewolf.Command
import Game.Werewolf.Engine   hiding (isWerewolvesTurn)
import Game.Werewolf.Game
import Game.Werewolf.Response

-- | Options.
data Options = Options
    { argTarget :: Text
    } deriving (Eq, Show)

-- | Handle.
handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options targetName) = do
    unlessM doesGameExist $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame

    let command = (if isWerewolvesTurn game
            then voteDevourCommand
            else voteLynchCommand
            ) callerName targetName

    case runExcept (runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game) of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game', messages) -> writeGame game' >> exitWith success { messages = messages }
