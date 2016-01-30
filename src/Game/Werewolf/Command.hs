{-|
Module      : Game.Werewolf.Command
Description : Command data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Command data structures.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Game.Werewolf.Command (
    -- * Command
    Command(..),

    -- ** Instances
    devourVoteCommand, lynchVoteCommand, noopCommand, pingCommand, quitCommand, seeCommand,
    statusCommand,
) where

import Control.Lens         hiding (only)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import qualified Data.Map  as Map
import           Data.Text (Text)

import Game.Werewolf.Engine
import Game.Werewolf.Game     hiding (isGameOver, isSeersTurn, isVillagesTurn, isWerewolvesTurn,
                               killPlayer)
import Game.Werewolf.Player   hiding (doesPlayerExist)
import Game.Werewolf.Response

data Command = Command { apply :: forall m . (MonadError [Message] m, MonadState Game m, MonadWriter [Message] m) => m () }

devourVoteCommand :: Text -> Text -> Command
devourVoteCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWerewolf callerName)           $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWerewolvesTurn                        $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName
    whenM (isPlayerWerewolf targetName)             $ throwError [playerCannotDevourAnotherWerewolf callerName]

    votes %= Map.insert callerName targetName

    aliveWerewolves <- uses players $ filterAlive . filterWerewolves

    tell $ map (\werewolf -> playerMadeDevourVoteMessage (werewolf ^. name) callerName targetName) aliveWerewolves

lynchVoteCommand :: Text -> Text -> Command
lynchVoteCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM isVillagesTurn                          $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName

    votes %= Map.insert callerName targetName

noopCommand :: Command
noopCommand = Command $ return ()

pingCommand :: Command
pingCommand = Command $ use stage >>= \stage' -> case stage' of
    GameOver        -> return ()
    SeersTurn       -> do
        seer <- uses players $ head . filterAlive . filterSeers

        tell [pingSeerMessage]
        tell [pingPlayerMessage seer]
    Sunrise         -> return ()
    Sunset          -> return ()
    VillagesTurn    -> do
        pendingVoters <- getPendingVoters

        tell [waitingOnMessage Nothing pendingVoters]
        tell $ map pingPlayerMessage pendingVoters
    WerewolvesTurn  -> do
        pendingVoters <- getPendingVoters

        tell [pingWerewolvesMessage]
        tell $ map pingPlayerMessage (filterWerewolves pendingVoters)

quitCommand :: Text -> Command
quitCommand callerName = Command $ do
    validatePlayer callerName callerName

    caller <- uses players $ findByName_ callerName

    killPlayer caller
    tell [playerQuitMessage caller]

    when (isSeer caller) $ see .= Nothing
    votes %= Map.delete callerName

seeCommand :: Text -> Text -> Command
seeCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerSeer callerName)       $ throwError [playerCannotDoThatMessage callerName]
    unlessM isSeersTurn                     $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName

    see .= Just targetName

statusCommand :: Text -> Command
statusCommand callerName = Command $ use stage >>= \stage' -> case stage' of
    GameOver        -> get >>= tell . gameOverMessages
    SeersTurn       -> do
        game <- get

        tell $ standardStatusMessages stage' (game ^. players)
    Sunrise         -> return ()
    Sunset          -> return ()
    VillagesTurn    -> do
        game            <- get
        pendingVoters   <- getPendingVoters

        tell $ standardStatusMessages stage' (game ^. players)
        tell [waitingOnMessage (Just callerName) pendingVoters]
    WerewolvesTurn  -> do
        game            <- get
        pendingVoters   <- filterWerewolves <$> getPendingVoters

        tell $ standardStatusMessages stage' (game ^. players)
        whenM (doesPlayerExist callerName &&^ isPlayerWerewolf callerName) $
            tell [waitingOnMessage (Just callerName) pendingVoters]
    where
        standardStatusMessages stage players =
            currentStageMessages callerName stage ++ [
            rolesInGameMessage (Just callerName) $ map _role players,
            playersInGameMessage callerName players
            ]

validatePlayer :: (MonadError [Message] m, MonadState Game m) => Text -> Text -> m ()
validatePlayer callerName name = do
    whenM isGameOver                $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist name)  $ throwError [playerDoesNotExistMessage callerName name]
    whenM (isPlayerDead name)       $ throwError [if callerName == name then playerIsDeadMessage callerName else targetIsDeadMessage callerName name]
