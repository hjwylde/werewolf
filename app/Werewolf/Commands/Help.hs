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

import           Data.List
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
handle callerName (Options (Just Commands)) = exitWith success { messages = map (privateMessage [callerName]) [
    "Usage: COMMAND ARG ...",
    "",
    "Available commands:",
    "  end - end the current game",
    "  help - help documents",
    "  start - start a new game",
    "  vote - vote against a player",
    "",
    "End:",
    "Usage: end",
    "  Ends the current game.",
    "",
    "Start:",
    "Usage: start PLAYER ...",
    "  Starts a new game with the given players. A game requires at least 7 players.",
    "",
    "Vote:",
    "Usage: vote PLAYER",
    "  Vote against a player. A townsperson may vote at daytime to lynch someone and a werewolf may vote at nighttime to kill a villager."
    ] }
handle callerName (Options (Just Description)) = exitWith success { messages = map (privateMessage [callerName]) [
    "Deep in the American countryside, the little town of Millers Hollow has recently been infiltrated by werewolves.",
    "Each night, murders are committed by the villagers, who due to some mysterious phenomenon (possibly the greenhouse effect) have become werewolves.",
    "It is now time to take control and eliminate this ancient evil, before the town loses its last few inhabitants.",
    "",
    "Objective of the Game:",
    "For the villagers: kill all of the werewolves.",
    "For the werewolves: kill all of the villagers."
    ] }
handle callerName (Options (Just Roles)) = exitWith success { messages = map (privateMessage [callerName])
    $ intercalate [""] $ map (\role -> [
        T.snoc (role ^. Role.name) ':',
        role ^. description,
        role ^. advice
        ]) allRoles
    }
handle callerName (Options (Just Rules)) = exitWith success { messages = map (privateMessage [callerName]) [
    "Each night, the werewolves bite, kill and devour one villager. During the day they try to conceal their identity and vile deeds from the villagers. Depending upon the number of players and variants used in the game, there are 1, 2, 3 or 4 werewolves in play.",
    "Each day, the survivors gather in the town square and try to discover who the werewolves are. This is done by studying the other player's social behaviours for hidden signs of lycanthropy. After discussing and debating, the villagers vote to lynch a suspect, who is then hanged, burned and eliminated from the game.",
    "",
    "Each player is informed of their role (see `help roles' for a list) at the start of the game. A game begins at night and follows a standard cycle.",
    "1. The town falls asleep.",
    "2. The werewolves wake up and select a victim.",
    "3. The town wakes up and find the victim.",
    "4. The town vote to lynch a suspect.",
    "The game is over when a single townsperson is left alive."
    ] }
handle callerName (Options Nothing) = exitWith success { messages = map (privateMessage [callerName]) [
    "Usage: help COMMAND",
    "",
    "Available commands:",
    "  commands - print the in-game commands",
    "  description - print the game description",
    "  rules - print the game rules",
    "  roles - print the roles and their description"
    ] }
