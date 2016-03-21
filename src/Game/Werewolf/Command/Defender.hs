{-|
Module      : Game.Werewolf.Command.Defender
Description : Defender commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Defender commands.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Command.Defender (
    -- ** Instances
    protectCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)

import Data.Text (Text)

import Game.Werewolf.Command
import Game.Werewolf.Game     hiding (doesPlayerExist, getPendingVoters, getVoteResult, killPlayer)
import Game.Werewolf.Messages
import Game.Werewolf.Util

protectCommand :: Text -> Text -> Command
protectCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerDefender callerName)                           $ throwError [playerCannotDoThatMessage callerName]
    unlessM isDefendersTurn                                         $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName
    whenM (has (priorProtect . traverse . only targetName) <$> get) $ throwError [playerCannotProtectSamePlayerTwiceInARowMessage callerName]

    priorProtect    .= Just targetName
    protect         .= Just targetName
