{-|
Module      : Game.Werewolf.Command.Villager
Description : Villager commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Villager commands.
-}

module Game.Werewolf.Command.Villager (
    -- * Commands
    voteCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Writer

import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Messages
import Game.Werewolf.Util

voteCommand :: Text -> Text -> Command
voteCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    whenM (uses allowedVoters (callerName `notElem`))   $ throwError [playerCannotDoThatMessage callerName]
    unlessM isVillagesTurn                              $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenM (isJust <$> getPlayerVote callerName)         $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName

    votes %= Map.insert callerName targetName

    whenJustM (preuse $ players . crookedSenators . alive) $ \crookedSenator ->
        tell [playerMadeLynchVoteMessage (Just $ crookedSenator ^. name) callerName targetName]
