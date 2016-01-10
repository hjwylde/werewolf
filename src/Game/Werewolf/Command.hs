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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Game.Werewolf.Command (
    -- * Command
    Command(..),

    -- ** Instances
    seeCommand, killVoteCommand, lynchVoteCommand,
) where

import Control.Lens         hiding (only)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List.Extra
import qualified Data.Map        as Map
import           Data.Text       (Text)

import           Game.Werewolf.Engine
import           Game.Werewolf.Game     hiding (isGameOver, isSeersTurn, isVillagersTurn,
                                         isWerewolvesTurn)
import           Game.Werewolf.Player   hiding (doesPlayerExist)
import qualified Game.Werewolf.Player   as Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role     as Role

data Command = Command { apply :: forall m . (MonadError [Message] m, MonadState Game m, MonadWriter [Message] m) => m () }

seeCommand :: Text -> Text -> Command
seeCommand callerName targetName = Command $ do
    whenM isGameOver                        $ throwError [gameIsOverMessage callerName]
    unlessM isSeersTurn                     $ throwError [playerCannotDoThatMessage callerName]
    unlessM (doesPlayerExist callerName)    $ throwError [playerDoesNotExistMessage callerName callerName]
    unlessM (doesPlayerExist targetName)    $ throwError [playerDoesNotExistMessage callerName targetName]

    caller <- uses players (findByName_ callerName)
    target <- uses players (findByName_ targetName)

    when (isDead caller) $ throwError [playerIsDeadMessage callerName]
    when (isDead target) $ throwError [targetIsDeadMessage callerName targetName]

    unless (isSeer caller) $ throwError [playerCannotDoThatMessage callerName]

    whenJustM (getPlayerSee caller) $ const (throwError [playerHasAlreadySeenMessage callerName])

    turn . sees %= Map.insert callerName targetName

    seersCount <- uses players (length . filterAlive . filterSeers)
    votes      <- use $ turn . votes

    when (seersCount == Map.size votes) $ do
        forM_ (Map.toList votes) $ \(seerName, targetName) -> do
            target <- uses players (findByName_ targetName)

            tell [playerSeenMessage seerName target]

        turn .= newWerewolvesTurn
        tell werewolvesTurnMessages

killVoteCommand :: Text -> Text -> Command
killVoteCommand callerName targetName = Command $ do
    whenM isGameOver                        $ throwError [gameIsOverMessage callerName]
    unlessM isWerewolvesTurn                $ throwError [playerCannotDoThatMessage callerName]
    unlessM (doesPlayerExist callerName)    $ throwError [playerDoesNotExistMessage callerName callerName]
    unlessM (doesPlayerExist targetName)    $ throwError [playerDoesNotExistMessage callerName targetName]

    caller <- uses players (findByName_ callerName)
    target <- uses players (findByName_ targetName)

    when (isDead caller) $ throwError [playerIsDeadMessage callerName]
    when (isDead target) $ throwError [targetIsDeadMessage callerName targetName]

    unless (isWerewolf caller) $ throwError [playerCannotDoThatMessage callerName]

    whenJustM (getPlayerVote caller) $ const (throwError [playerHasAlreadyVotedMessage callerName])

    turn . votes %= Map.insert callerName targetName

    werewolvesCount <- uses players (length . filterAlive . filterWerewolves)
    votes           <- use $ turn . votes

    when (werewolvesCount == Map.size votes) $ do
        werewolfNames <- uses players (map Player._name . filterWerewolves)
        tell $ map (uncurry $ playerMadeKillVoteMessage werewolfNames) (Map.toList votes)

        turn .= newVillagersTurn
        tell villagersTurnMessages

        let mTargetName = only . last $ groupSortOn (length . flip elemIndices (Map.elems votes)) (nub $ Map.elems votes)
        case mTargetName of
            Nothing         -> tell [noPlayerKilledMessage]
            Just targetName -> do
                target <- uses players (findByName_ targetName)

                killPlayer target
                tell [playerKilledMessage (target ^. Player.name) (target ^. Player.role . Role.name)]

lynchVoteCommand :: Text -> Text -> Command
lynchVoteCommand callerName targetName = Command $ do
    whenM isGameOver                        $ throwError [gameIsOverMessage callerName]
    unlessM isVillagersTurn                 $ throwError [playerCannotDoThatMessage callerName]
    unlessM (doesPlayerExist callerName)    $ throwError [playerDoesNotExistMessage callerName callerName]
    unlessM (doesPlayerExist targetName)    $ throwError [playerDoesNotExistMessage callerName targetName]

    caller <- uses players (findByName_ callerName)
    target <- uses players (findByName_ targetName)

    when (isDead caller) $ throwError [playerIsDeadMessage callerName]
    when (isDead target) $ throwError [targetIsDeadMessage callerName targetName]

    whenJustM (getPlayerVote caller) $ const (throwError [playerHasAlreadyVotedMessage callerName])

    turn . votes %= Map.insert callerName targetName

    playersCount    <- uses players (length . filterAlive)
    votes           <- use $ turn . votes

    when (playersCount == Map.size votes) $ do
        tell $ map (uncurry playerMadeLynchVoteMessage) (Map.toList votes)

        let mLynchedName = only . last $ groupSortOn (length . flip elemIndices (Map.elems votes)) (nub $ Map.elems votes)
        case mLynchedName of
            Nothing             -> tell [noPlayerLynchedMessage]
            Just lynchedName    -> do
                target <- uses players (findByName_ lynchedName)

                killPlayer target
                tell [playerLynchedMessage (target ^. Player.name) (target ^. Player.role . Role.name)]

        turn .= newSeersTurn
        use players >>= tell . seersTurnMessages . filterSeers

only :: [a] -> Maybe a
only [a]    = Just a
only _      = Nothing
