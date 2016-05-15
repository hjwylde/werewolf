{-|
Module      : Game.Werewolf.Command.Witch
Description : Witch commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Witch commands.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Command.Witch (
    -- * Commands
    healCommand, passCommand, poisonCommand,
) where

import Control.Lens
import Control.Lens.Extra
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State

import Data.Map  as Map
import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Command
import Game.Werewolf.Message.Error
import Game.Werewolf.Util

healCommand :: Text -> Command
healCommand callerName = Command $ do
    validateCommand callerName
    whenM (use healUsed)        $ throwError [playerHasAlreadyHealedMessage callerName]
    whenM (hasn'tuse votee)     $ throwError [playerCannotDoThatRightNowMessage callerName]

    healUsed    .= True
    votes       .= Map.empty

passCommand :: Text -> Command
passCommand callerName = Command $ do
    validateCommand callerName

    passed .= True

poisonCommand :: Text -> Text -> Command
poisonCommand callerName targetName = Command $ do
    validateCommand callerName
    whenM (use poisonUsed)                      $ throwError [playerHasAlreadyPoisonedMessage callerName]
    validatePlayer callerName targetName
    whenM (hasuse $ votee . named targetName)   $ throwError [playerCannotDoThatMessage callerName]

    poison      .= Just targetName
    poisonUsed  .= True

validateCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateCommand callerName = do
    validatePlayer callerName callerName
    unlessM (isPlayerWitch callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWitchsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
