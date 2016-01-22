{-|
Module      : Game.Werewolf.Command
Description : Command data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Command data structures.
-}

{-# LANGUAGE FlexibleContexts      #-}
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
import Data.List
import           Data.Text (Text)
import           qualified Data.Text as T

import Game.Werewolf.Engine
import Game.Werewolf.Role hiding (Werewolves, Villagers, name, _name, findByName_)
import Game.Werewolf.Player hiding (doesPlayerExist)
import Game.Werewolf.Game     hiding (isGameOver, isSeersTurn, isVillagersTurn, isWerewolvesTurn,
                               killPlayer)
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

lynchVoteCommand :: Text -> Text -> Command
lynchVoteCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM isVillagersTurn                         $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName

    votes %= Map.insert callerName targetName

noopCommand :: Command
noopCommand = Command $ return ()

pingCommand :: Command
pingCommand = Command $ use turn >>= \turn' -> case turn' of
    Seers       -> tell [pingSeersMessage]
    Villagers   -> do
        game <- get

        tell [waitingOnMessage Nothing $ filter (flip Map.notMember (game ^. votes) . _name) (filterAlive $ game ^. players)]
    Werewolves  -> tell [pingWerewolvesMessage]
    NoOne       -> return ()

quitCommand :: Text -> Command
quitCommand callerName = Command $ do
    validatePlayer callerName callerName

    caller <- uses players $ findByName_ callerName

    killPlayer caller
    tell [playerQuitMessage caller]

    sees    %= Map.delete callerName
    votes   %= Map.delete callerName

seeCommand :: Text -> Text -> Command
seeCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerSeer callerName)           $ throwError [playerCannotDoThatMessage callerName]
    unlessM isSeersTurn                         $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenJustM (getPlayerSee callerName) . const $ throwError [playerHasAlreadySeenMessage callerName]
    validatePlayer callerName targetName

    sees %= Map.insert callerName targetName

statusCommand :: Text -> Command
statusCommand callerName = Command $ use turn >>= \turn' -> case turn' of
    Seers       -> do
        game <- get

        tell $ standardStatusMessages turn' (game ^. players)
    Villagers   -> do
        game <- get

        tell $ standardStatusMessages turn' (game ^. players)
        tell [waitingOnMessage (Just [callerName]) $ filter (flip Map.notMember (game ^. votes) . _name) (filterAlive $ game ^. players)]
    Werewolves  -> do
        unlessM (doesPlayerExist callerName) $ throwError [playerDoesNotExistMessage callerName callerName]

        game <- get

        tell $ standardStatusMessages turn' (game ^. players)
        whenM (isPlayerWerewolf callerName) $ tell [waitingOnMessage (Just [callerName]) $ filter (flip Map.notMember (game ^. votes) . _name) (filterAlive . filterWerewolves $ game ^. players)]
    NoOne       -> do
        aliveAllegiances <- uses players $ nub . map (_allegiance . _role) . filterAlive

        case aliveAllegiances of
            [allegiance]    -> tell [gameOverMessage . Just . T.pack $ show allegiance]
            _               -> tell [gameOverMessage Nothing]
    where
        standardStatusMessages turn players = [
            currentTurnMessage callerName turn,
            rolesInGameMessage (Just [callerName]) $ map _role players,
            playersInGameMessage callerName players
            ]

validatePlayer :: (MonadError [Message] m, MonadState Game m) => Text -> Text -> m ()
validatePlayer callerName name = do
    whenM isGameOver                $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist name)  $ throwError [playerDoesNotExistMessage callerName name]
    whenM (isPlayerDead name)       $ throwError [if callerName == name then playerIsDeadMessage callerName else targetIsDeadMessage callerName name]
