{-|
Module      : Game.Werewolf.Command.Werewolf
Description : Werewolf commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Werewolf commands.
-}

module Game.Werewolf.Command.Werewolf (
    -- * Commands
    voteCommand,

    -- ** Validation
    validatePlayer,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import Game.Werewolf
import Game.Werewolf.Messages
import Game.Werewolf.Util

voteCommand :: Text -> Text -> Command
voteCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWerewolf callerName)       $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWerewolvesTurn                    $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenM (isJust <$> getPlayerVote callerName) $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName
    whenM (isPlayerWerewolf targetName)         $ throwError [playerCannotDevourAnotherWerewolfMessage callerName]

    votes %= Map.insert callerName targetName

    aliveWerewolfNames <- toListOf (players . werewolves . alive . name) <$> get

    tell [playerMadeDevourVoteMessage werewolfName callerName targetName | werewolfName <- aliveWerewolfNames \\ [callerName]]
