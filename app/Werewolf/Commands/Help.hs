{-|
Module      : Werewolf.Commands.Help
Description : Options and handler for the help subcommand.

Copyright   : (c) Henry J. Wylde, 2016
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
import Control.Monad.Extra
import Control.Monad.IO.Class

import           Data.Function
import           Data.List
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Game.Werewolf      hiding (Command)
import qualified Game.Werewolf.Role as Role

import Werewolf.Game

data Options = Options
    { argCommand :: Maybe Command
    } deriving (Eq, Show)

data Command = Commands | Rules | Roles
    deriving (Eq, Show)

handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options (Just Commands)) = do
    mGame <- ifM doesGameExist (Just <$> readGame) (return Nothing)

    exitWith success
        { messages = map (privateMessage callerName) (commandsMessages callerName mGame)
        }
handle callerName (Options (Just Roles)) = do
    roles <- (sortBy (compare `on` view Role.name) . nub) <$> ifM doesGameExist
        (toListOf (players . roles) <$> readGame)
        (return allRoles)

    exitWith success
        { messages = map (privateMessage callerName . roleMessage) roles
        }
handle callerName (Options (Just Rules)) = exitWith success
    { messages = map (privateMessage callerName) rulesMessages
    }
handle callerName (Options Nothing) = exitWith success
    { messages = map (privateMessage callerName) helpMessages
    }

commandsMessages :: Text -> Maybe Game -> [Text]
commandsMessages callerName mGame = map (T.intercalate "\n") $ filter (/= [])
    [ [ "Global commands:"
      , "- `start ([--extra-roles ROLE,...] | [--random-extra-roles]) PLAYER...`"
      , "- `end`"
      , "- `quit`"
      , "- `version`"
      ]
    , [ "Status commands:"
      , "- `ping` ping the status of the current game publicly"
      , "- `status` get the status of the current game"
      , "- `circle [--include-dead]` get the game circle"
      ]
    , [ "Standard commands:"
      , "- `vote PLAYER`"
      ]
    , ifPlayerHasRole callerName mGame defenderRole
      [ "Defender commands:"
      , "- `protect PLAYER`"
      ]
    , ifPlayerHasRole callerName mGame scapegoatRole
      [ "Scapegoat commands:"
      , "- `choose PLAYER,...`"
      ]
    , ifPlayerHasRole callerName mGame seerRole
      [ "Seer commands:"
      , "- `see PLAYER`"
      ]
    , ifPlayerHasRole callerName mGame wildChildRole
      [ "Wild-child commands:"
      , "- `choose PLAYER`"
      ]
    , ifPlayerHasRole callerName mGame witchRole
      [ "Witch commands:"
      , "- `heal`"
      , "- `poison PLAYER`"
      , "- `pass`"
      ]
    , ifPlayerHasRole callerName mGame wolfHoundRole
      [ "Wolf-hound commands:"
      , "- `choose (villagers | werewolves)`"
      ]
    ]

roleMessage :: Role -> Text
roleMessage role = T.intercalate "\n"
    [ T.concat [role ^. Role.name, " (", T.pack . show $ role ^. balance, "):"]
    , role ^. description
    , role ^. advice
    ]

rulesMessages :: [Text]
rulesMessages = map (T.intercalate "\n")
    [ [ T.unwords
        [ "Each night, the Werewolves bite, kill and devour one Villager."
        , "During the day they try to conceal their identity and vile deeds from the Villagers."
        , "The number of Werewolves in play depends upon"
        , " the number of players and variants used in the game."
        ]
      , T.unwords
        [ "Each day,"
        , "the survivors gather in the town square and try to discover who the Werewolves are."
        , "This is done by studying the other player's social behaviours"
        , "for hidden signs of lycanthropy."
        , "After discussing and debating, the village votes to lynch a suspect,"
        , "who is then hanged, burned and eliminated from the game."
        ]
      ]
    , [ T.unwords
        [ "Each player is informed of their role (see `help roles' for a list)"
        , "at the start of the game. A game begins at night and follows a standard cycle."
        , "(N.B., when the Angel is in play the game begins with the village vote.)"
        ]
      , "1. (When the Angel is in play) the village votes to lynch a suspect."
      , "2. The village falls asleep."
      , "3. (First round only) the Wild-child wakes up and chooses a role model."
      , "4. The Defender wakes up and protects someone."
      , "5. The Seer wakes up and sees someone's allegiance."
      , "6. (First round only) the Wolf-hound wakes up and chooses an allegiance."
      , "7. The Werewolves wake up and select a victim."
      , "8. The Witch wakes up and may heal the victim and/or poison someone."
      , "9. The village wakes up and find the victim."
      , "10. The village votes to lynch a suspect."
      , "11. (When the Scapegoat is blamed) the Scapegot chooses whom may vote on the next day."
      , T.unwords
        [ "The game is over when only Villagers or Werewolves are left alive,"
        , "or when one of the Loners completes their own objective."
        ]
      ]
    ]

helpMessages :: [Text]
helpMessages = map (T.intercalate "\n")
    [ [ T.unwords
        [ "Deep in the American countryside,"
        , "the little town of Millers Hollow has recently been infiltrated by Werewolves."
        ]
      , T.unwords
        [ "Each night, murders are committed by the Villagers,"
        , "who due to some mysterious phenomenon (possibly the greenhouse effect)"
        , "have become Werewolves."
        ]
      , T.unwords
        [ "It is now time to take control and eliminate this ancient evil,"
        , "before the town loses its last few inhabitants."
        ]
      ]
    , [ "Help commands:"
      , "- `help commands`"
      , "- `help roles`"
      , "- `help rules`"
      ]
    ]

ifPlayerHasRole :: Monoid m => Text -> Maybe Game -> Role -> m -> m
ifPlayerHasRole _ Nothing _ m                   = m
ifPlayerHasRole callerName (Just game) role' m
     | has (players . names . only callerName) game
        && has (role . only role') player           = m
    | otherwise                                     = mempty
     where
        player = game ^?! players . traverse . filteredBy name callerName

--ifRoleInPlay :: Monoid m => Maybe Game -> Role -> m -> m
--ifRoleInPlay Nothing _ m        = m
--ifRoleInPlay (Just game) role' m
--    | has (players . roles . only role') game   = m
--    | otherwise                                 = mempty
--
