{-|
Module      : Werewolf.Commands.Help
Description : Options and handler for the help subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the help subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Commands.Help (
    -- * Options
    Options(..), Command(..),

    -- * Handle
    handle,
) where

import Control.Lens
import Control.Monad.IO.Class

import           Data.Text (Text)
import qualified Data.Text as T

import Game.Werewolf.Response
import Game.Werewolf.Role     as Role

-- | Options.
data Options = Options
    { argCommand :: Maybe Command
    } deriving (Eq, Show)

-- | Command.
data Command = Commands | Description | Rules | Roles
    deriving (Eq, Show)

-- | Handle.
handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options (Just Commands)) = exitWith success {
    messages = map (privateMessage [callerName]) commandsMessages
    }
handle callerName (Options (Just Description)) = exitWith success {
    messages = map (privateMessage [callerName]) descriptionMessages
    }
handle callerName (Options (Just Roles)) = exitWith success {
    messages = map (\role -> privateMessage [callerName] $ T.intercalate "\n" [
        T.snoc (role ^. Role.name) ':',
        role ^. description,
        role ^. advice
        ]) allRoles
    }
handle callerName (Options (Just Rules)) = exitWith success {
    messages = map (privateMessage [callerName]) rulesMessages
    }
handle callerName (Options Nothing) = exitWith success {
    messages = map (privateMessage [callerName]) helpMessages
    }

commandsMessages :: [Text]
commandsMessages = map (T.intercalate "\n") [[
    "end",
    "- Ends the current game."
    ], [
    "ping",
    "- Pings the status of the current game publicly."
    ], [
    "quit",
    "- Quit the current game."
    ], [
    "see PLAYER",
    "- See a player's allegiance. A Seer may determine a player's allegiance once per day."
    ], [
    "start [--extra-roles ROLE,...] PLAYER ...",
    "- Starts a new game with the given players and extra roles. A game requires at least 7 players."
    ], [
    "status",
    "- Gets the status of the current game."
    ], [
    "vote PLAYER",
    T.unwords [
        "- Vote against a player.",
        "A townsperson may vote at daytime to lynch someone",
        "and a Werewolf may vote at nighttime to devour a Villager."
        ]
    ]]

descriptionMessages :: [Text]
descriptionMessages = map (T.intercalate "\n") [[
    T.unwords [
        "Deep in the American countryside,",
        "the little town of Millers Hollow has recently been infiltrated by Werewolves."
        ],
    T.unwords [
        "Each night, murders are committed by the Villagers,",
        "who due to some mysterious phenomenon (possibly the greenhouse effect)",
        "have become Werewolves."
        ],
    T.unwords [
        "It is now time to take control and eliminate this ancient evil,",
        "before the town loses its last few inhabitants."
        ]
    ], [
    "Objective of the Game:",
    "For the Villagers: lynch all of the Werewolves.",
    "For the Werewolves: devour all of the Villagers."
    ]]

rulesMessages :: [Text]
rulesMessages = map (T.intercalate "\n") [[
    T.unwords [
        "Each night, the Werewolves bite, kill and devour one Villager.",
        "During the day they try to conceal their identity and vile deeds from the Villagers.",
        "Depending upon the number of players and variants used in the game,",
        "there are 1, 2, 3 or 4 Werewolves in play."
        ],
    T.unwords [
        "Each day,",
        "the survivors gather in the town square and try to discover who the Werewolves are.",
        "This is done by studying the other player's social behaviours",
        "for hidden signs of lycanthropy.",
        "After discussing and debating, the village votes to lynch a suspect,",
        "who is then hanged, burned and eliminated from the game."
        ]
    ], [
    T.unwords [
        "Each player is informed of their role (see `help roles' for a list)",
        "at the start of the game. A game begins at night and follows a standard cycle."
        ],
    "1. The village falls asleep.",
    "2. The Seers wake up and each see someone.",
    "3. The Werewolves wake up and select a victim.",
    "4. The village wakes up and find the victim.",
    "5. The village votes to lynch a suspect.",
    "The game is over when only Villagers or Werewolves are left alive."
    ]]

helpMessages :: [Text]
helpMessages = map (T.intercalate "\n") [[
    "help commands",
    "- Print the in-game commands."
    ], [
    "help description",
    "- Print the game description."
    ], [
    "help roles",
    "- Print the roles and their description."
    ], [
    "help rules",
    "- Print the game rules."
    ]]
