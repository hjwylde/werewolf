{-|
Module      : Game.Werewolf.Command
Description : Command data structure.

Copyright   : (c) Henry J. Wylde, 2016
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
    chooseAllegianceCommand, choosePlayerCommand, choosePlayersCommand, circleCommand, healCommand,
    noopCommand, passCommand, pingCommand, poisonCommand, protectCommand, quitCommand, seeCommand,
    statusCommand, voteDevourCommand, voteLynchCommand,
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
import qualified Data.Text  as T

import           Game.Werewolf.Engine
import           Game.Werewolf.Internal.Game   hiding (doesPlayerExist, getDevourEvent,
                                                getPendingVoters, getPlayerVote, killPlayer,
                                                setPlayerRole)
import           Game.Werewolf.Internal.Player
import           Game.Werewolf.Internal.Role   hiding (angel, name)
import qualified Game.Werewolf.Internal.Role   as Role
import           Game.Werewolf.Messages
import           Game.Werewolf.Response

data Command = Command { apply :: forall m . (MonadError [Message] m, MonadState Game m, MonadWriter [Message] m) => m () }

chooseAllegianceCommand :: Text -> Text -> Command
chooseAllegianceCommand callerName allegianceName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWolfHound callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWolfHoundsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (isNothing mRole)                  $ throwError [allegianceDoesNotExistMessage callerName allegianceName]

    setPlayerRole callerName (fromJust mRole)
    where
        mRole = case T.toLower allegianceName of
            "villagers"     -> Just simpleVillagerRole
            "werewolves"    -> Just simpleWerewolfRole
            _               -> Nothing

choosePlayerCommand :: Text -> Text -> Command
choosePlayerCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWildChild callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWildChildsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (callerName == targetName)         $ throwError [playerCannotChooseSelfMessage callerName]
    validatePlayer callerName targetName

    roleModel .= Just targetName

choosePlayersCommand :: Text -> [Text] -> Command
choosePlayersCommand callerName targetNames = Command $ do
    whenM isGameOver                        $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist callerName)    $ throwError [playerDoesNotExistMessage callerName callerName]
    unlessM (isPlayerScapegoat callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isScapegoatsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (null targetNames)                 $ throwError [playerMustChooseAtLeastOneTargetMessage callerName]
    when (callerName `elem` targetNames)    $ throwError [playerCannotChooseSelfMessage callerName]
    forM_ targetNames $ validatePlayer callerName
    whenM (use villageIdiotRevealed &&^ anyM isPlayerVillageIdiot targetNames) $
        throwError [playerCannotChooseVillageIdiotMessage callerName]

    allowedVoters   .= targetNames
    scapegoatBlamed .= False

circleCommand :: Text -> Bool -> Command
circleCommand callerName includeDead = Command $ do
        players' <- gets $ toListOf (players . traverse . if includeDead then id else alive)

        tell [circleMessage callerName players']

healCommand :: Text -> Command
healCommand callerName = Command $ do
    validateWitchsCommand callerName
    whenM (use healUsed)                    $ throwError [playerHasAlreadyHealedMessage callerName]
    whenM (isNothing <$> getDevourEvent)    $ throwError [playerCannotDoThatRightNowMessage callerName]

    heal        .= True
    healUsed    .= True

noopCommand :: Command
noopCommand = Command $ return ()

passCommand :: Text -> Command
passCommand callerName = Command $ do
    validateWitchsCommand callerName

    passes %= nub . cons callerName

pingCommand :: Command
pingCommand = Command $ use stage >>= \stage' -> case stage' of
    GameOver        -> return ()
    DefendersTurn   -> do
        defender <- findPlayerByRole_ defenderRole

        tell [pingRoleMessage $ defender ^. role . Role.name]
        tell [pingPlayerMessage $ defender ^. name]
    ScapegoatsTurn  -> do
        scapegoat <- findPlayerByRole_ scapegoatRole

        tell [pingRoleMessage $ scapegoat ^. role . Role.name]
        tell [pingPlayerMessage $ scapegoat ^. name]
    SeersTurn       -> do
        seer <- findPlayerByRole_ seerRole

        tell [pingRoleMessage $ seer ^. role . Role.name]
        tell [pingPlayerMessage $ seer ^. name]
    Sunrise         -> return ()
    Sunset          -> return ()
    UrsussGrunt     -> return ()
    VillagesTurn    -> do
        allowedVoterNames <- use allowedVoters
        pendingVoterNames <- toListOf names <$> getPendingVoters

        tell [waitingOnMessage Nothing $ allowedVoterNames `intersect` pendingVoterNames]
        tell $ map pingPlayerMessage (allowedVoterNames `intersect` pendingVoterNames)
    WerewolvesTurn  -> do
        pendingVoters <- getPendingVoters

        tell [pingRoleMessage "Werewolves"]
        tell $ map pingPlayerMessage (pendingVoters ^.. werewolves . name)
    WildChildsTurn  -> do
        wildChild <- findPlayerByRole_ wildChildRole

        tell [pingRoleMessage $ wildChild ^. role . Role.name]
        tell [pingPlayerMessage $ wildChild ^. name]
    WitchsTurn      -> do
        witch <- findPlayerByRole_ witchRole

        tell [pingRoleMessage $ witch ^. role . Role.name]
        tell [pingPlayerMessage $ witch ^. name]
    WolfHoundsTurn  -> do
        wolfHound <- findPlayerByRole_ wolfHoundRole

        tell [pingRoleMessage $ wolfHound ^. role . Role.name]
        tell [pingPlayerMessage $ wolfHound ^. name]

poisonCommand :: Text -> Text -> Command
poisonCommand callerName targetName = Command $ do
    validateWitchsCommand callerName
    whenM (use poisonUsed)              $ throwError [playerHasAlreadyPoisonedMessage callerName]
    validatePlayer callerName targetName
    whenJustM getDevourEvent            $ \(DevourEvent targetName') ->
        when (targetName == targetName') $ throwError [playerCannotDoThatMessage callerName]

    poison      .= Just targetName
    poisonUsed  .= True

protectCommand :: Text -> Text -> Command
protectCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerDefender callerName)   $ throwError [playerCannotDoThatMessage callerName]
    unlessM isDefendersTurn                 $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName
    whenJustM (use priorProtect) $ \priorName ->
        when (targetName == priorName) $ throwError [playerCannotProtectSamePlayerTwiceInARowMessage callerName]

    priorProtect    .= Just targetName
    protect         .= Just targetName

quitCommand :: Text -> Command
quitCommand callerName = Command $ do
    validatePlayer callerName callerName

    caller <- findPlayerByName_ callerName

    killPlayer callerName
    tell [playerQuitMessage caller]

    passes %= delete callerName
    when (is angel caller)      $ setPlayerRole callerName simpleVillagerRole
    when (is defender caller)   $ do
        protect         .= Nothing
        priorProtect    .= Nothing
    when (is seer caller)       $ see .= Nothing
    when (is wildChild caller)  $ roleModel .= Nothing
    when (is witch caller)      $ do
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
    Sunrise         -> return ()
    Sunset          -> return ()
    VillagesTurn    -> do
        game                <- get
        allowedVoterNames   <- use allowedVoters
        pendingVoterNames   <- toListOf names <$> getPendingVoters

        tell $ standardStatusMessages stage' (game ^. players)
        tell [waitingOnMessage (Just callerName) (allowedVoterNames `intersect` pendingVoterNames)]
    WerewolvesTurn  -> do
        game                <- get
        pendingVoterNames   <- toListOf (werewolves . name) <$> getPendingVoters

        tell $ standardStatusMessages stage' (game ^. players)
        whenM (doesPlayerExist callerName &&^ isPlayerWerewolf callerName) $
            tell [waitingOnMessage (Just callerName) pendingVoterNames]
    _               -> do
        game <- get

        tell $ standardStatusMessages stage' (game ^. players)
    where
        standardStatusMessages stage players =
            currentStageMessages callerName stage ++ [playersInGameMessage callerName players]

voteDevourCommand :: Text -> Text -> Command
voteDevourCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWerewolf callerName)           $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWerewolvesTurn                        $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenJustM (getPlayerVote callerName) . const    $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName
    whenM (isPlayerWerewolf targetName)             $ throwError [playerCannotDevourAnotherWerewolfMessage callerName]

    votes %= Map.insert callerName targetName

    aliveWerewolfNames <- gets $ toListOf (players . werewolves . alive . name)

    tell $ map (\werewolfName -> playerMadeDevourVoteMessage werewolfName callerName targetName) (aliveWerewolfNames \\ [callerName])

voteLynchCommand :: Text -> Text -> Command
voteLynchCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    whenM (uses allowedVoters (callerName `notElem`))                       $ throwError [playerCannotDoThatMessage callerName]
    unlessM isVillagesTurn                                                  $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenJustM (getPlayerVote callerName) . const                            $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName
    whenM (use villageIdiotRevealed &&^ isPlayerVillageIdiot targetName)    $ throwError [playerCannotLynchVillageIdiotMessage callerName]

    votes %= Map.insert callerName targetName

validatePlayer :: (MonadError [Message] m, MonadState Game m) => Text -> Text -> m ()
validatePlayer callerName name = do
    whenM isGameOver                $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist name)  $ throwError [playerDoesNotExistMessage callerName name]
    whenM (isPlayerDead name)       $ throwError [if callerName == name then playerIsDeadMessage callerName else targetIsDeadMessage callerName name]

validateWitchsCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateWitchsCommand callerName = do
    validatePlayer callerName callerName
    unlessM (isPlayerWitch callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWitchsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
