{-|
Module      : Werewolf.Command.Help
Description : Options and handler for the help subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the help subcommand.
-}

module Werewolf.Command.Help (
    -- * Options
    Options(..), Command(..),

    -- * Handle
    handle,
) where

import Control.Lens
import Control.Monad.Extra
import Control.Monad.IO.Class

import Data.Function
import Data.List
import Data.Maybe
import Data.String.Humanise
import Data.Text            (Text)

import Game.Werewolf
import Game.Werewolf.Message.Command

import Werewolf.System

data Options = Options
    { argCommand :: Maybe Command
    } deriving (Eq, Show)

data Command = Commands
    { optAll :: Bool
    } | Rules
    { optAll :: Bool
    } | Roles
    { optAll :: Bool
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Text -> Options -> m ()
handle callerName tag (Options (Just (Commands optAll))) = do
    mGame <- getGame tag optAll

    exitWith success
        { messages = commandsMessages callerName mGame
        }
handle callerName tag (Options (Just (Roles optAll))) = do
    mGame       <- getGame tag optAll
    let mGame'  = mGame >>= \game -> if has (variant . noRoleKnowledge) game
            then Nothing
            else Just game

    let roles' = sortBy (compare `on` humanise) . nub $ maybe allRoles (toListOf $ players . roles) mGame'

    exitWith success
        { messages = map (roleMessage callerName) roles'
        }
handle callerName tag (Options (Just (Rules optAll))) = do
    mGame       <- getGame tag optAll
    let mGame'  = mGame >>= \game -> if has (variant . noRoleKnowledge) game
            then Nothing
            else Just game

    exitWith success
        { messages = rulesMessages callerName mGame'
        }
handle callerName _ (Options Nothing) = exitWith success
    { messages = helpMessages callerName
    }

commandsMessages :: Text -> Maybe Game -> [Message]
commandsMessages callerName mGame =
    [ globalCommandsMessage callerName
    , statusCommandsMessage callerName
    , standardCommandsMessage callerName
    ] ++ [ hunterCommandsMessage callerName
    | isNothing mGame || has (players . hunters . named callerName) (fromJust mGame)
    ] ++ [ necromancerCommandsMessage callerName
    | isNothing mGame || has (players . necromancers . named callerName) (fromJust mGame)
    ] ++ [ oracleCommandsMessage callerName
    | isNothing mGame || has (players . oracles . named callerName) (fromJust mGame)
    ] ++ [ orphanCommandsMessage callerName
    | isNothing mGame || has (players . orphans . named callerName) (fromJust mGame)
    ] ++ [ protectorCommandsMessage callerName
    | isNothing mGame || has (players . protectors . named callerName) (fromJust mGame)
    ] ++ [ scapegoatCommandsMessage callerName
    | isNothing mGame || has (players . scapegoats . named callerName) (fromJust mGame)
    ] ++ [ seerCommandsMessage callerName
    | isNothing mGame || has (players . seers . named callerName) (fromJust mGame)
    ] ++ [ witchCommandsMessage callerName
    | isNothing mGame || has (players . witches . named callerName) (fromJust mGame)
    ]

rulesMessages :: Text -> Maybe Game -> [Message]
rulesMessages callerName mGame =
    [ rulesMessage callerName
    , stagesMessage callerName mGame
    ]

helpMessages :: Text -> [Message]
helpMessages callerName =
    [ gameDescriptionMessage callerName
    , helpCommandsMessage callerName
    ]

getGame :: MonadIO m => Text -> Bool -> m (Maybe Game)
getGame _ True  = return Nothing
getGame tag _   = ifM (doesGameExist tag) (Just <$> readGame tag) (return Nothing)
