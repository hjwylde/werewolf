{-|
Module      : Game.Werewolf.Command.Seer
Description : Seer commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Seer commands.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Command.Seer (
    -- * Commands
    seeCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra

import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Messages
import Game.Werewolf.Util

seeCommand :: Text -> Text -> Command
seeCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerSeer callerName)       $ throwError [playerCannotDoThatMessage callerName]
    unlessM isSeersTurn                     $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName

    see .= Just targetName
