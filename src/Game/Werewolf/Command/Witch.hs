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
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Messages
import Game.Werewolf.Util

healCommand :: Text -> Command
healCommand callerName = Command $ do
    validateCommand callerName
    whenM (use healUsed)                                        $ throwError [playerHasAlreadyHealedMessage callerName]
    whenM (hasn't (events . traverse . _DevourEvent) <$> get)   $ throwError [playerCannotDoThatRightNowMessage callerName]

    heal        .= True
    healUsed    .= True

passCommand :: Text -> Command
passCommand callerName = Command $ do
    validateCommand callerName

    passed .= True

poisonCommand :: Text -> Text -> Command
poisonCommand callerName targetName = Command $ do
    validateCommand callerName
    whenM (use poisonUsed)                                                      $ throwError [playerHasAlreadyPoisonedMessage callerName]
    validatePlayer callerName targetName
    whenM (has (events . traverse . _DevourEvent . only targetName) <$> get)    $ throwError [playerCannotDoThatMessage callerName]

    poison      .= Just targetName
    poisonUsed  .= True

validateCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateCommand callerName = do
    validatePlayer callerName callerName
    unlessM (isPlayerWitch callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWitchsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
