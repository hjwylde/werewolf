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
{-# LANGUAGE Rank2Types            #-}

module Game.Werewolf.Command (
    -- * Command
    Command(..),

    -- ** Instances
    bootCommand, chooseAllegianceCommand, choosePlayerCommand, choosePlayersCommand, circleCommand,
    healCommand, noopCommand, passDevotedServantsTurnCommand, passWitchsTurnCommand, pingCommand,
    poisonCommand, protectCommand, quitCommand, revealCommand, seeCommand, statusCommand,
    voteDevourCommand, voteLynchCommand,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

import           Game.Werewolf.Game     hiding (doesPlayerExist, getPendingVoters, getVoteResult,
                                         killPlayer)
import           Game.Werewolf.Messages
import           Game.Werewolf.Player
import           Game.Werewolf.Response
import           Game.Werewolf.Role     hiding (name)
import qualified Game.Werewolf.Role     as Role
import           Game.Werewolf.Util

data Command = Command { apply :: forall m . (MonadError [Message] m, MonadState Game m, MonadWriter [Message] m) => m () }

bootCommand :: Text -> Text -> Command
bootCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    validatePlayer callerName targetName
    whenM (uses (boots . at targetName) $ elem callerName . maybe [] id) $
        throwError [playerHasAlreadyVotedToBootMessage callerName targetName]

    boots %= Map.insertWith (++) targetName [callerName]

    tell [playerVotedToBootMessage callerName targetName]

chooseAllegianceCommand :: Text -> Text -> Command
chooseAllegianceCommand callerName allegianceName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWolfHound callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWolfHoundsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
    when (isNothing mAllegiance)            $ throwError [allegianceDoesNotExistMessage callerName allegianceName]

    allegianceChosen .= mAllegiance
    where
        mAllegiance = case T.toLower allegianceName of
            "villagers"     -> Just Villagers
            "werewolves"    -> Just Werewolves
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
        players' <- toListOf (players . traverse . if includeDead then id else alive) <$> get

        tell [circleMessage callerName players']

healCommand :: Text -> Command
healCommand callerName = Command $ do
    validateWitchsCommand callerName
    whenM (use healUsed)                                        $ throwError [playerHasAlreadyHealedMessage callerName]
    whenM (hasn't (events . traverse . _DevourEvent) <$> get)   $ throwError [playerCannotDoThatRightNowMessage callerName]

    heal        .= True
    healUsed    .= True

noopCommand :: Command
noopCommand = Command $ return ()

passDevotedServantsTurnCommand :: Text -> Command
passDevotedServantsTurnCommand callerName = Command $ do
    validateDevotedServantsCommand callerName

    passes %= nub . cons callerName

passWitchsTurnCommand :: Text -> Command
passWitchsTurnCommand callerName = Command $ do
    validateWitchsCommand callerName

    passes %= nub . cons callerName

pingCommand :: Text -> Command
pingCommand callerName = Command $ use stage >>= \stage' -> case stage' of
    DefendersTurn       -> do
        defender <- findPlayerBy_ role defenderRole

        tell [pingRoleMessage $ defenderRole ^. Role.name]
        tell [pingPlayerMessage $ defender ^. name]
    DevotedServantsTurn -> do
        devotedServant <- findPlayerBy_ role devotedServantRole

        tell [pingRoleMessage $ devotedServantRole ^. Role.name]
        tell [pingPlayerMessage $ devotedServant ^. name]
    GameOver            -> tell [gameIsOverMessage callerName]
    Lynching            -> return ()
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
    UrsussGrunt         -> return ()
    VillagesTurn        -> do
        allowedVoterNames <- use allowedVoters
        pendingVoterNames <- toListOf names <$> getPendingVoters

        tell [waitingOnMessage Nothing $ allowedVoterNames `intersect` pendingVoterNames]
        tell $ map pingPlayerMessage (allowedVoterNames `intersect` pendingVoterNames)
    WerewolvesTurn      -> do
        pendingVoters <- getPendingVoters

        tell [pingRoleMessage "Werewolves"]
        tell $ map pingPlayerMessage (pendingVoters ^.. werewolves . name)
    WildChildsTurn      -> do
        wildChild <- findPlayerBy_ role wildChildRole

        tell [pingRoleMessage $ wildChildRole ^. Role.name]
        tell [pingPlayerMessage $ wildChild ^. name]
    WitchsTurn          -> do
        witch <- findPlayerBy_ role witchRole

        tell [pingRoleMessage $ witchRole ^. Role.name]
        tell [pingPlayerMessage $ witch ^. name]
    WolfHoundsTurn      -> do
        wolfHound <- findPlayerBy_ role wolfHoundRole

        tell [pingRoleMessage $ wolfHoundRole ^. Role.name]
        tell [pingPlayerMessage $ wolfHound ^. name]

poisonCommand :: Text -> Text -> Command
poisonCommand callerName targetName = Command $ do
    validateWitchsCommand callerName
    whenM (use poisonUsed)                                                      $ throwError [playerHasAlreadyPoisonedMessage callerName]
    validatePlayer callerName targetName
    whenM (has (events . traverse . _DevourEvent . only targetName) <$> get)    $ throwError [playerCannotDoThatMessage callerName]

    poison      .= Just targetName
    poisonUsed  .= True

protectCommand :: Text -> Text -> Command
protectCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerDefender callerName)                           $ throwError [playerCannotDoThatMessage callerName]
    unlessM isDefendersTurn                                         $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName
    whenM (has (priorProtect . traverse . only targetName) <$> get) $ throwError [playerCannotProtectSamePlayerTwiceInARowMessage callerName]

    priorProtect    .= Just targetName
    protect         .= Just targetName

quitCommand :: Text -> Command
quitCommand callerName = Command $ do
    validatePlayer callerName callerName

    caller <- findPlayerBy_ name callerName

    tell [playerQuitMessage caller]

    removePlayer callerName

revealCommand :: Text -> Command
revealCommand callerName = Command $ do
    validateDevotedServantsCommand callerName

    target <- head <$> getVoteResult

    let targetRole = target ^. role
    let targetName = target ^. name

    setPlayerRole callerName targetRole
    setPlayerRole targetName devotedServantRole

    tell [devotedServantRevealedMessage callerName]

    resetRole targetRole
    where
        resetRole role
            | role == simpleWerewolfRole    = do
                aliveWerewolfNames <- toListOf (players . werewolves . alive . name) <$> get

                tell $ devotedServantJoinedPackMessages callerName aliveWerewolfNames
            | role == villageIdiotRole      = villageIdiotRevealed .= False
            | role == wildChildRole         = roleModel .= Nothing
            | role == witchRole             = healUsed .= False >> poisonUsed .= False
            | role == wolfHoundRole         = allegianceChosen .= Nothing
            | otherwise                     = return ()

seeCommand :: Text -> Text -> Command
seeCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerSeer callerName)       $ throwError [playerCannotDoThatMessage callerName]
    unlessM isSeersTurn                     $ throwError [playerCannotDoThatRightNowMessage callerName]
    validatePlayer callerName targetName

    see .= Just targetName

statusCommand :: Text -> Command
statusCommand callerName = Command $ use stage >>= \stage' -> case stage' of
    GameOver        -> tell [gameIsOverMessage callerName]
    Lynching        -> return ()
    Sunrise         -> return ()
    Sunset          -> return ()
    UrsussGrunt     -> return ()
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

voteDevourCommand :: Text -> Text -> Command
voteDevourCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    unlessM (isPlayerWerewolf callerName)       $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWerewolvesTurn                    $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenM (isJust <$> getPlayerVote callerName) $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName
    whenM (isPlayerWerewolf targetName)         $ throwError [playerCannotDevourAnotherWerewolfMessage callerName]

    votes %= Map.insert callerName targetName

    aliveWerewolfNames <- toListOf (players . werewolves . alive . name) <$> get

    tell [playerMadeDevourVoteMessage werewolfName callerName targetName | werewolfName <- aliveWerewolfNames \\ [callerName]]

voteLynchCommand :: Text -> Text -> Command
voteLynchCommand callerName targetName = Command $ do
    validatePlayer callerName callerName
    whenM (uses allowedVoters (callerName `notElem`))   $ throwError [playerCannotDoThatMessage callerName]
    unlessM isVillagesTurn                              $ throwError [playerCannotDoThatRightNowMessage callerName]
    whenM (isJust <$> getPlayerVote callerName)         $ throwError [playerHasAlreadyVotedMessage callerName]
    validatePlayer callerName targetName

    votes %= Map.insert callerName targetName

validatePlayer :: (MonadError [Message] m, MonadState Game m) => Text -> Text -> m ()
validatePlayer callerName name = do
    whenM isGameOver                $ throwError [gameIsOverMessage callerName]
    unlessM (doesPlayerExist name)  $ throwError [playerDoesNotExistMessage callerName name]
    whenM (isPlayerDead name)       $ throwError [if callerName == name then playerIsDeadMessage callerName else targetIsDeadMessage callerName name]

validateDevotedServantsCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateDevotedServantsCommand callerName = do
    validatePlayer callerName callerName
    unlessM (isPlayerDevotedServant callerName) $ throwError [playerCannotDoThatMessage callerName]
    unlessM isDevotedServantsTurn               $ throwError [playerCannotDoThatRightNowMessage callerName]

validateWitchsCommand :: (MonadError [Message] m, MonadState Game m) => Text -> m ()
validateWitchsCommand callerName = do
    validatePlayer callerName callerName
    unlessM (isPlayerWitch callerName)  $ throwError [playerCannotDoThatMessage callerName]
    unlessM isWitchsTurn                $ throwError [playerCannotDoThatRightNowMessage callerName]
