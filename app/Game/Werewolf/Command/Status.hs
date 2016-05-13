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
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Text (Text)

import Game.Werewolf          hiding (getPendingVoters)
import Game.Werewolf.Command
import Game.Werewolf.Messages
import Game.Werewolf.Util

circleCommand :: Text -> Bool -> Command
circleCommand callerName includeDead = Command $ do
        players' <- toListOf (players . traverse . if includeDead then id else alive) <$> get

        tell [circleMessage callerName players']

pingCommand :: Text -> Command
pingCommand callerName = Command $ use stage >>= \stage' -> case stage' of
    FerinasGrunt        -> return ()
    GameOver            -> tell [gameIsOverMessage callerName]
    HuntersTurn1        -> pingRole hunterRole
    HuntersTurn2        -> pingRole hunterRole
    Lynching            -> return ()
    OraclesTurn         -> pingRole oracleRole
    OrphansTurn         -> pingRole orphanRole
    ProtectorsTurn      -> pingRole protectorRole
    ScapegoatsTurn      -> pingRole scapegoatRole
    SeersTurn           -> pingRole seerRole
    Sunrise             -> return ()
    Sunset              -> return ()
    VillageDrunksTurn   -> pingRole villageDrunkRole
    VillagesTurn        -> pingVillagers
    WerewolvesTurn      -> pingWerewolves
    WitchsTurn          -> pingRole witchRole

pingRole :: (MonadState Game m, MonadWriter [Message] m) => Role -> m ()
pingRole role' = do
    player <- findPlayerBy_ role role'

    tell [pingRoleMessage role']
    tell [pingPlayerMessage $ player ^. name]

pingVillagers :: (MonadState Game m, MonadWriter [Message] m) => m ()
pingVillagers = do
    allowedVoterNames <- use allowedVoters
    pendingVoterNames <- toListOf names <$> getPendingVoters

    tell [pingVillageMessage]
    tell $ map pingPlayerMessage (allowedVoterNames `intersect` pendingVoterNames)

pingWerewolves :: (MonadState Game m, MonadWriter [Message] m) => m ()
pingWerewolves = do
    pendingVoters <- getPendingVoters

    tell [pingWerewolvesMessage]
    tell $ map pingPlayerMessage (pendingVoters ^.. werewolves . name)

statusCommand :: Text -> Command
statusCommand callerName = Command $ use stage >>= \stage' -> case stage' of
    GameOver    -> tell [gameIsOverMessage callerName]
    _           -> tell . statusMessages stage' =<< use players
    where
        statusMessages stage players =
            currentStageMessages callerName stage ++
            [ rolesInGameMessage (Just callerName) (players ^.. roles)
            , playersInGameMessage callerName players
            ]
