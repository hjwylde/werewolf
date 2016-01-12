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
                                         isWerewolvesTurn, killPlayer)
import           Game.Werewolf.Player   hiding (doesPlayerExist)
import qualified Game.Werewolf.Player   as Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role     as Role

data Command = Command { apply :: forall m . (MonadError [Message] m, MonadState Game m, MonadWriter [Message] m) => m () }

seeCommand :: Text -> Text -> Command
seeCommand callerName targetName = Command $ do
    validateArguments callerName targetName

    unlessM isSeersTurn                         $ throwError [playerCannotDoThatMessage callerName]
    unlessM (isPlayerSeer callerName)           $ throwError [playerCannotDoThatMessage callerName]
    whenJustM (getPlayerSee callerName) . const $ throwError [playerHasAlreadySeenMessage callerName]

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
    validateArguments callerName targetName

    unlessM isWerewolvesTurn                        $ throwError [playerCannotDoThatMessage callerName]
    unlessM (isPlayerWerewolf callerName)           $ throwError [playerCannotDoThatMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]

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
    validateArguments callerName targetName

    unlessM isVillagersTurn                         $ throwError [playerCannotDoThatMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]

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

validateArguments :: (MonadError [Message] m, MonadState Game m) => Text -> Text -> m ()
validateArguments callerName targetName = do
    whenM isGameOver                        $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist callerName)    $ throwError [playerDoesNotExistMessage callerName callerName]
    unlessM (doesPlayerExist targetName)    $ throwError [playerDoesNotExistMessage callerName targetName]

    whenM (isPlayerDead callerName) $ throwError [playerIsDeadMessage callerName]
    whenM (isPlayerDead targetName) $ throwError [targetIsDeadMessage callerName targetName]

only :: [a] -> Maybe a
only [a]    = Just a
only _      = Nothing
