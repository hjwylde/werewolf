{-|
Module      : Werewolf.Command.Start
Description : Options and handler for the start subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the start subcommand.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Werewolf.Command.Start (
    -- * Options
    Options(..), ExtraRoles(..),

    -- * Handle
    handle,
) where

import Control.Lens
import Control.Lens.Extra
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Text (Text)

import Game.Werewolf
import Game.Werewolf.Engine
import Game.Werewolf.Message.Error

import System.Random.Shuffle

import Werewolf.System

data Options = Options
    { optExtraRoles  :: ExtraRoles
    , optIncludeSeer :: Bool
    , argPlayers     :: [Text]
    } deriving (Eq, Show)

data ExtraRoles = None | Random | Use [Text]
    deriving (Eq, Show)

handle :: (MonadIO m, MonadRandom m) => Text -> Text -> Options -> m ()
handle callerName tag (Options extraRoles includeSeer playerNames) = do
    whenM (doesGameExist tag &&^ (hasn't (stage . _GameOver) <$> readGame tag)) $ exitWith failure
        { messages = [gameAlreadyRunningMessage callerName]
        }

    result <- runExceptT $ do
        extraRoles' <- case extraRoles of
            None            -> return []
            Random          -> randomExtraRoles $ length playerNames
            Use roleNames   -> useExtraRoles callerName roleNames

        let extraRoles''    = if includeSeer then nub (seerRole:extraRoles') else extraRoles'
        let roles           = padRoles extraRoles'' (length playerNames + 1)

        players <- createPlayers (callerName:playerNames) <$> shuffleM roles

        runWriterT $ startGame callerName players >>= execStateT checkStage

    case result of
        Left errorMessages      -> exitWith failure { messages = errorMessages }
        Right (game, messages)  -> writeOrDeleteGame tag game >> exitWith success { messages = messages }

randomExtraRoles :: MonadRandom m => Int -> m [Role]
randomExtraRoles n = do
    let minimum = n `div` 4 + 1
    let maximum = n `div` 3 + 1

    count <- getRandomR (minimum, maximum)

    take count <$> shuffleM restrictedRoles

useExtraRoles :: MonadError [Message] m => Text -> [Text] -> m [Role]
useExtraRoles callerName roleNames = forM roleNames $ \roleName -> case findByTag roleName of
    Just role   -> return role
    Nothing     -> throwError [roleDoesNotExistMessage callerName roleName]

findByTag :: Text -> Maybe Role
findByTag tag' = restrictedRoles ^? traverse . filteredBy tag tag'

padRoles :: [Role] -> Int -> [Role]
padRoles roles n = roles ++ simpleVillagerRoles ++ simpleWerewolfRoles
    where
        goal                    = 3
        m                       = max (n - length roles) 0
        startingBalance         = sumOf (traverse . balance) roles
        simpleWerewolfBalance   = simpleWerewolfRole ^. balance

        -- Little magic here to calculate how many Werewolves and Villagers we want.
        -- This tries to ensure that the balance of the game is between -2 and 2.
        simpleWerewolvesCount   = (goal - m - startingBalance) `div` (simpleWerewolfBalance - 1) + 1
        simpleVillagersCount    = m - simpleWerewolvesCount

        -- N.B., if roles is quite unbalanced then one list will be empty.
        simpleVillagerRoles = replicate simpleVillagersCount simpleVillagerRole
        simpleWerewolfRoles = replicate simpleWerewolvesCount simpleWerewolfRole

createPlayers :: [Text] -> [Role] -> [Player]
createPlayers = zipWith newPlayer
