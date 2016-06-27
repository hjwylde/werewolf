{-|
Module      : Game.Werewolf.Command.Necromancer
Description : Seer commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Necromancer commands.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Command.Necromancer (
    -- * Commands
    passCommand, raiseCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Message.Command
import Game.Werewolf.Message.Error
import Game.Werewolf.Util

passCommand :: Text -> Command
passCommand callerName = Command $ do
    validateCommand callerName

    passed .= True

raiseCommand :: Text -> Command
raiseCommand callerName = Command $ do
    validateCommand callerName

    players . traverse . dead . role .= zombieRole
    players . traverse . dead . state .= Alive
    tell . deadRaisedMessages =<< get

    deadRaised .= True

validateCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateCommand callerName = do
    validatePlayer callerName callerName
    unlessM (isPlayerNecromancer callerName)    $ throwError [playerCannotDoThatMessage callerName]
    unlessM isNecromancersTurn                  $ throwError [playerCannotDoThatRightNowMessage callerName]
