{-|
Module      : Game.Werewolf.Command.Werewolf
Description : Werewolf commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Werewolf commands.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Command.Werewolf (
    -- * Commands
    unvoteCommand, voteCommand,

    -- ** Validation
    validatePlayer,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Messages
import Game.Werewolf.Util

unvoteCommand :: Text -> Command
unvoteCommand callerName = Command $ do
    validateCommand callerName
    whenM (isNothing <$> getPlayerVote callerName) $ throwError [playerHasNotVotedMessage callerName]

    votes %= Map.delete callerName

    aliveWerewolfNames <- toListOf (players . werewolves . alive . name) <$> get

    tell [playerRescindedVoteMessage werewolfName callerName | werewolfName <- aliveWerewolfNames \\ [callerName]]

voteCommand :: Text -> Text -> Command
voteCommand callerName targetName = Command $ do
    validateCommand callerName
    whenM (isJust <$> getPlayerVote callerName) $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName
    whenM (isPlayerWerewolf targetName)         $ throwError [playerCannotDevourAnotherWerewolfMessage callerName]

    votes %= Map.insert callerName targetName

    aliveWerewolfNames  <- toListOf (players . werewolves . alive . name) <$> get
    caller              <- findPlayerBy_ name callerName
    target              <- findPlayerBy_ name targetName

    tell [playerMadeDevourVoteMessage werewolfName caller target | werewolfName <- aliveWerewolfNames \\ [callerName]]

validateCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateCommand callerName = do
    validatePlayer callerName callerName
    unlessM (isPlayerWerewolf callerName)   $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWerewolvesTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
