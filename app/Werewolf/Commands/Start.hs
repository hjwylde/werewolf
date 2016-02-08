{-|
Module      : Werewolf.Commands.Start
Description : Options and handler for the start subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the start subcommand.
-}

module Werewolf.Commands.Start (
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

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T

import Game.Werewolf.Engine   hiding (isGameOver)
import Game.Werewolf.Game
import Game.Werewolf.Response
import Game.Werewolf.Role

-- | Options.
data Options = Options
    { optExtraRoleNames :: [Text]
    , argPlayers        :: [Text]
    } deriving (Eq, Show)

-- | Handle.
handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options extraRoleNames playerNames) = do
    whenM (doesGameExist &&^ fmap (not . isGameOver) readGame) $ exitWith failure
        { messages = [gameAlreadyRunningMessage callerName]
        }

    result <- runExceptT $ do
        extraRoles <- forM extraRoleNames $ \roleName -> maybe
            (throwError [roleDoesNotExistMessage callerName roleName])
            return
            (findByName roleName)

        players <- createPlayers (callerName:playerNames) extraRoles

        runWriterT $ startGame callerName players >>= execStateT checkStage

    case result of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeGame game >> exitWith success { messages = messages }


findByName :: Text -> Maybe Role
findByName name' = find ((name' ==) . T.toLower . view name) allRoles
