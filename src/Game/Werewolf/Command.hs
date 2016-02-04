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
    devourVoteCommand, healCommand, lynchVoteCommand, noopCommand, passCommand, pingCommand,
    poisonCommand, protectCommand, quitCommand, seeCommand, statusCommand,
) where

import Control.Lens         hiding (only)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)

import Game.Werewolf.Engine
import Game.Werewolf.Game     hiding (getDevourEvent, getPendingVoters, getPlayerVote,
                               isDefendersTurn, isGameOver, isSeersTurn, isVillagesTurn,
                               isWerewolvesTurn, isWitchsTurn, killPlayer)
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
    whenM (isPlayerWerewolf targetName)             $ throwError [playerCannotDevourAnotherWerewolfMessage callerName]

    votes %= Map.insert callerName targetName

    aliveWerewolfNames <- uses players $ map _name . filterAlive . filterWerewolves

    tell $ map (\werewolfName -> playerMadeDevourVoteMessage werewolfName callerName targetName) (aliveWerewolfNames \\ [callerName])

healCommand :: Text -> Command
healCommand callerName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWitch callerName)      $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWitchsTurn                    $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenM (use healUsed)                    $ throwError [playerHasAlreadyHealedMessage callerName]
    whenM (isNothing <$> getDevourEvent)    $ throwError [playerCannotDoThatRightNowMessage callerName]

    heal        .= True
    healUsed    .= True

lynchVoteCommand :: Text -> Text -> Command
lynchVoteCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM isVillagesTurn                          $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName

    votes %= Map.insert callerName targetName

noopCommand :: Command
noopCommand = Command $ return ()

passCommand :: Text -> Command
passCommand callerName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWitch callerName)      $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWitchsTurn                    $ throwError [playerCannotDoThatRightNowMessage callerName]

    passes %= nub . cons callerName

pingCommand :: Command
pingCommand = Command $ use stage >>= \stage' -> case stage' of
    GameOver        -> return ()
    DefendersTurn   -> do
        defender <- uses players $ head . filterDefenders

        tell [pingDefenderMessage]
        tell [pingPlayerMessage $ defender ^. name]
    SeersTurn       -> do
        seer <- uses players $ head . filterSeers

        tell [pingSeerMessage]
        tell [pingPlayerMessage $ seer ^. name]
    Sunrise         -> return ()
    Sunset          -> return ()
    VillagesTurn    -> do
        pendingVoters <- getPendingVoters

        tell [waitingOnMessage Nothing pendingVoters]
        tell $ map (pingPlayerMessage . _name) pendingVoters
    WerewolvesTurn  -> do
        pendingVoters <- getPendingVoters

        tell [pingWerewolvesMessage]
        tell $ map (pingPlayerMessage . _name) (filterWerewolves pendingVoters)
    WitchsTurn      -> do
        witch <- uses players $ head . filterWitches

        tell [pingWitchMessage]
        tell [pingPlayerMessage $ witch ^. name]

poisonCommand :: Text -> Text -> Command
poisonCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWitch callerName)      $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWitchsTurn                    $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenM (use poisonUsed)                  $ throwError [playerHasAlreadyPoisonedMessage callerName]
    validatePlayer callerName targetName
    whenJustM getDevourEvent                $ \(DevourEvent targetName') ->
        when (targetName == targetName') $ throwError [playerCannotDoThatMessage callerName]

    poison      .= Just targetName
    poisonUsed  .= True

protectCommand :: Text -> Text -> Command
protectCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerDefender callerName)   $ throwError [playerCannotDoThatMessage callerName]
    unlessM isDefendersTurn                 $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (callerName == targetName)         $ throwError [playerCannotProtectSelfMessage callerName]
    validatePlayer callerName targetName
    whenJustM (use priorProtect) $ \priorName ->
        when (targetName == priorName) $ throwError [playerCannotProtectSamePlayerTwiceInARowMessage callerName]

    priorProtect    .= Just targetName
    protect         .= Just targetName

quitCommand :: Text -> Command
quitCommand callerName = Command $ do
    validatePlayer callerName callerName

    caller <- uses players $ findByName_ callerName

    killPlayer caller
    tell [playerQuitMessage caller]

    passes %= delete callerName
    when (isDefender caller)    $ do
        protect         .= Nothing
        priorProtect    .= Nothing
    when (isSeer caller)        $ see .= Nothing
    when (isWitch caller)       $ do
        heal        .= False
        healUsed    .= False
        poison      .= Nothing
        poisonUsed  .= False
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
    DefendersTurn   -> do
        game <- get

        tell $ standardStatusMessages stage' (game ^. players)
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
    WitchsTurn      -> do
        game <- get

        tell $ standardStatusMessages stage' (game ^. players)
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
