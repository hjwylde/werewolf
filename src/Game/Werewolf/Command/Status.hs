{-|
Module      : Game.Werewolf.Command.Status
Description : Status commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Status commands.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Game.Werewolf.Command.Status (
    -- * Commands
    circleCommand, pingCommand, statusCommand,
) where

import Control.Lens
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import Data.List
import Data.Text (Text)

import           Game.Werewolf          hiding (doesPlayerExist, getPendingVoters)
import           Game.Werewolf.Messages
import qualified Game.Werewolf.Role     as Role
import           Game.Werewolf.Util

circleCommand :: Text -> Bool -> Command
circleCommand callerName includeDead = Command $ do
        players' <- toListOf (players . traverse . if includeDead then id else alive) <$> get

        tell [circleMessage callerName players']

pingCommand :: Text -> Command
pingCommand callerName = Command $ use stage >>= \stage' -> case stage' of
    FerinasGrunt    -> return ()
    GameOver        -> tell [gameIsOverMessage callerName]
    HuntersTurn1    -> pingRole hunterRole
    HuntersTurn2    -> pingRole hunterRole
    Lynching        -> return ()
    OrphansTurn     -> pingRole orphanRole
    ProtectorsTurn  -> pingRole protectorRole
    ScapegoatsTurn  -> pingRole scapegoatRole
    SeersTurn       -> pingRole seerRole
    Sunrise         -> return ()
    Sunset          -> return ()
    VillagesTurn    -> pingVillagers
    WerewolvesTurn  -> pingWerewolves
    WitchsTurn      -> pingRole witchRole

pingRole :: (MonadState Game m, MonadWriter [Message] m) => Role -> m ()
pingRole role' = do
    player <- findPlayerBy_ role role'

    tell [pingRoleMessage $ role' ^. Role.name]
    tell [pingPlayerMessage $ player ^. name]

pingVillagers :: (MonadState Game m, MonadWriter [Message] m) => m ()
pingVillagers = do
    allowedVoterNames <- use allowedVoters
    pendingVoterNames <- toListOf names <$> getPendingVoters

    tell [waitingOnMessage Nothing $ allowedVoterNames `intersect` pendingVoterNames]
    tell $ map pingPlayerMessage (allowedVoterNames `intersect` pendingVoterNames)

pingWerewolves :: (MonadState Game m, MonadWriter [Message] m) => m ()
pingWerewolves = do
    pendingVoters <- getPendingVoters

    tell [pingRoleMessage "Werewolves"]
    tell $ map pingPlayerMessage (pendingVoters ^.. werewolves . name)

statusCommand :: Text -> Command
statusCommand callerName = Command $ use stage >>= \stage' -> case stage' of
    FerinasGrunt    -> return ()
    GameOver        -> tell [gameIsOverMessage callerName]
    Lynching        -> return ()
    Sunrise         -> return ()
    Sunset          -> return ()
    VillagesTurn    -> do
        allowedVoterNames <- use allowedVoters
        pendingVoterNames <- toListOf names <$> getPendingVoters

        tell . standardStatusMessages stage' =<< use players
        tell [waitingOnMessage (Just callerName) (allowedVoterNames `intersect` pendingVoterNames)]
    WerewolvesTurn  -> do
        pendingVoterNames <- toListOf (werewolves . name) <$> getPendingVoters

        tell . standardStatusMessages stage' =<< use players
        whenM (doesPlayerExist callerName &&^ isPlayerWerewolf callerName) $
            tell [waitingOnMessage (Just callerName) pendingVoterNames]
    _               -> tell . standardStatusMessages stage' =<< use players
    where
        standardStatusMessages stage players =
            currentStageMessages callerName stage ++
            [ rolesInGameMessage (Just callerName) (players ^.. roles)
            , playersInGameMessage callerName players
            ]
