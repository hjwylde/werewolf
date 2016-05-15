{-|
Module      : Game.Werewolf.Command
Description : Command data structure.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Command data structures.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

module Game.Werewolf.Command (
    -- * Command
    Command(..),

    -- ** Instances
    noopCommand,

    -- ** Validation
    validatePlayer,
) where

import Control.Lens.Extra
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Message.Command
import Game.Werewolf.Message.Error
import Game.Werewolf.Util

data Command = Command { apply :: forall m . (MonadError [Message] m, MonadState Game m, MonadWriter [Message] m) => m () }

noopCommand :: Command
noopCommand = Command $ return ()

validatePlayer :: (MonadError [Message] m, MonadState Game m) => Text -> Text -> m ()
validatePlayer callerName name' = do
    whenM isGameOver                $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist name') $ throwError [playerDoesNotExistMessage callerName name']

    player <- findPlayerBy_ name name'

    when (is dead player) $ throwError [if callerName == name' then playerIsDeadMessage callerName else targetIsDeadMessage callerName player]
