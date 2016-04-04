{-|
Module      : Game.Werewolf.Command.DevotedServant
Description : Devoted Servant commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Devoted Servant commands.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Werewolf.Command.DevotedServant (
    -- * Commands
    passCommand, revealCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import Data.List
import Data.Text (Text)

import Game.Werewolf          hiding (getVoteResult)
import Game.Werewolf.Messages
import Game.Werewolf.Util

passCommand :: Text -> Command
passCommand callerName = Command $ do
    validateCommand callerName

    passed .= True

revealCommand :: Text -> Command
revealCommand callerName = Command $ do
    validateCommand callerName

    target <- head <$> getVoteResult

    let targetRole = target ^. role
    let targetName = target ^. name

    setPlayerRole callerName targetRole
    setPlayerRole targetName devotedServantRole

    tell [devotedServantRevealedMessage callerName]

    resetRole callerName targetRole

validateCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateCommand callerName = do
    validatePlayer callerName callerName
    unlessM (isPlayerDevotedServant callerName) $ throwError [playerCannotDoThatMessage callerName]
    unlessM isDevotedServantsTurn               $ throwError [playerCannotDoThatRightNowMessage callerName]

resetRole :: (MonadState Game m, MonadWriter [Message] m) => Text -> Role -> m ()
resetRole callerName role
    | role == jesterRole            = jesterRevealed .= False
    | role == orphanRole            = roleModel .= Nothing
    | role == simpleWerewolfRole    = do
        aliveWerewolfNames <- toListOf (players . werewolves . alive . name) <$> get

        tell $ devotedServantJoinedPackMessages callerName (aliveWerewolfNames \\ [callerName])
    | role == witchRole             = healUsed .= False >> poisonUsed .= False
    | role == wolfHoundRole         = allegianceChosen .= False
    | otherwise                     = return ()
