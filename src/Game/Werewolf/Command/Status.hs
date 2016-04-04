{-|
Module      : Game.Werewolf.Command.Status
Description : Status commands.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Status commands.
-}

{-# LANGUAGE OverloadedStrings #-}

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
    DevotedServantsTurn -> do
        devotedServant <- findPlayerBy_ role devotedServantRole

        tell [pingRoleMessage $ devotedServantRole ^. Role.name]
        tell [pingPlayerMessage $ devotedServant ^. name]

    FerinasGrunt        -> return ()

    GameOver            -> tell [gameIsOverMessage callerName]

    Lynching            -> return ()

    OrphansTurn      -> do
        orphan <- findPlayerBy_ role orphanRole

        tell [pingRoleMessage $ orphanRole ^. Role.name]
        tell [pingPlayerMessage $ orphan ^. name]

    ProtectorsTurn      -> do
        protector <- findPlayerBy_ role protectorRole

        tell [pingRoleMessage $ protectorRole ^. Role.name]
        tell [pingPlayerMessage $ protector ^. name]

    ScapegoatsTurn      -> do
        scapegoat <- findPlayerBy_ role scapegoatRole

        tell [pingRoleMessage $ scapegoatRole ^. Role.name]
        tell [pingPlayerMessage $ scapegoat ^. name]

    SeersTurn           -> do
        seer <- findPlayerBy_ role seerRole

        tell [pingRoleMessage $ seerRole ^. Role.name]
        tell [pingPlayerMessage $ seer ^. name]

    Sunrise             -> return ()

    Sunset              -> return ()

    VillagesTurn        -> do
        allowedVoterNames <- use allowedVoters
        pendingVoterNames <- toListOf names <$> getPendingVoters

        tell [waitingOnMessage Nothing $ allowedVoterNames `intersect` pendingVoterNames]
        tell $ map pingPlayerMessage (allowedVoterNames `intersect` pendingVoterNames)

    WerewolvesTurn      -> do
        pendingVoters <- getPendingVoters

        tell [pingRoleMessage "Werewolves"]
        tell $ map pingPlayerMessage (pendingVoters ^.. werewolves . name)

    WitchsTurn          -> do
        witch <- findPlayerBy_ role witchRole

        tell [pingRoleMessage $ witchRole ^. Role.name]
        tell [pingPlayerMessage $ witch ^. name]

    WolfHoundsTurn      -> do
        wolfHound <- findPlayerBy_ role wolfHoundRole

        tell [pingRoleMessage $ wolfHoundRole ^. Role.name]
        tell [pingPlayerMessage $ wolfHound ^. name]

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
