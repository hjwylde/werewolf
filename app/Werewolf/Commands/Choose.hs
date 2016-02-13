{-|
Module      : Werewolf.Commands.Choose
Description : Options and handler for the choose subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the choose subcommand.
-}

module Werewolf.Commands.Choose (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T

import Game.Werewolf.Command
import Game.Werewolf.Engine
import Game.Werewolf.Response
import Game.Werewolf.Role

-- | Options.
data Options = Options
    { argAllegiance :: Text
    } deriving (Eq, Show)

-- | Handle.
handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options allegianceName) = do
    unlessM doesGameExist $ exitWith failure
        { messages = [noGameRunningMessage callerName]
        }

    game <- readGame

    let result = runExcept $ do
            allegiance  <- maybe (throwError [allegianceDoesNotExistMessage callerName allegianceName]) return (findByName allegianceName)
            let command = chooseCommand callerName allegiance

            runWriterT $ execStateT (apply command >> checkStage >> checkGameOver) game

    case result of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeGame game >> exitWith success { messages = messages }

findByName :: Text -> Maybe Allegiance
findByName name = find ((name ==) . T.toLower . T.pack . show) allAllegiances
