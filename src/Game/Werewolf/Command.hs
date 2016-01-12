{-|
Module      : Game.Werewolf.Command
Description : Command data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Command data structures.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Game.Werewolf.Command (
    -- * Command
    Command(..),

    -- ** Instances
    seeCommand, killVoteCommand, lynchVoteCommand,
) where

import Control.Lens         hiding (only)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import qualified Data.Map  as Map
import           Data.Text (Text)

import Game.Werewolf.Engine
import Game.Werewolf.Game     hiding (isGameOver, isSeersTurn, isVillagersTurn, isWerewolvesTurn,
                               killPlayer)
import Game.Werewolf.Response

data Command = Command { apply :: forall m . (MonadError [Message] m, MonadState Game m, MonadWriter [Message] m) => m () }

seeCommand :: Text -> Text -> Command
seeCommand callerName targetName = Command $ do
    validateArguments callerName targetName

    unlessM isSeersTurn                         $ throwError [playerCannotDoThatMessage callerName]
    unlessM (isPlayerSeer callerName)           $ throwError [playerCannotDoThatMessage callerName]
    whenJustM (getPlayerSee callerName) . const $ throwError [playerHasAlreadySeenMessage callerName]

    sees %= Map.insert callerName targetName

killVoteCommand :: Text -> Text -> Command
killVoteCommand callerName targetName = Command $ do
    validateArguments callerName targetName

    unlessM isWerewolvesTurn                        $ throwError [playerCannotDoThatMessage callerName]
    unlessM (isPlayerWerewolf callerName)           $ throwError [playerCannotDoThatMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]

    votes %= Map.insert callerName targetName

lynchVoteCommand :: Text -> Text -> Command
lynchVoteCommand callerName targetName = Command $ do
    validateArguments callerName targetName

    unlessM isVillagersTurn                         $ throwError [playerCannotDoThatMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]

    votes %= Map.insert callerName targetName

validateArguments :: (MonadError [Message] m, MonadState Game m) => Text -> Text -> m ()
validateArguments callerName targetName = do
    whenM isGameOver                        $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist callerName)    $ throwError [playerDoesNotExistMessage callerName callerName]
    unlessM (doesPlayerExist targetName)    $ throwError [playerDoesNotExistMessage callerName targetName]

    whenM (isPlayerDead callerName) $ throwError [playerIsDeadMessage callerName]
    whenM (isPlayerDead targetName) $ throwError [targetIsDeadMessage callerName targetName]
