{-|
Module      : Game.Werewolf.Command.Villager
Description : Villager commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Villager commands.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Command.Villager (
    -- * Commands
    unvoteCommand, voteCommand,
) where

import Control.Lens.Extra
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Message.Command
import Game.Werewolf.Message.Error
import Game.Werewolf.Util

unvoteCommand :: Text -> Command
unvoteCommand callerName = Command $ do
    validateCommand callerName
    whenM (isNothing <$> getPlayerVote callerName) $ throwError [playerHasNotVotedMessage callerName]

    votes %= Map.delete callerName

    caller <- findPlayerBy_ name callerName

    whenJustM (preuse $ players . crookedSenators . alive) $ \crookedSenator ->
        tell [playerRescindedVoteMessage (crookedSenator ^. name) caller]

voteCommand :: Text -> Text -> Command
voteCommand callerName targetName = Command $ do
    validateCommand callerName
    whenM (isJust <$> getPlayerVote callerName) $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName
    whenM (isPlayerZombie targetName)           $ throwError [playerCannotChooseZombieMessage callerName]

    votes %= Map.insert callerName targetName

    caller <- findPlayerBy_ name callerName
    target <- findPlayerBy_ name targetName

    whenJustM (preuse $ players . crookedSenators . alive) $ \crookedSenator ->
        tell [playerMadeLynchVoteMessage (Just $ crookedSenator ^. name) caller target]

validateCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateCommand callerName = do
    validatePlayer callerName callerName
    whenM (hasn'tuse $ allowedVoters . named callerName)    $ throwError [playerCannotDoThatMessage callerName]
    unlessM isVillagesTurn                                  $ throwError [playerCannotDoThatRightNowMessage callerName]
