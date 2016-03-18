{-|
Module      : Werewolf.Command.Help
Description : Options and handler for the help subcommand.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the help subcommand.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Command.Help (
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

data Command = Commands
    { optAll :: Bool
    } | Rules
    { optAll :: Bool
    } | Roles
    { optAll :: Bool
    } deriving (Eq, Show)

handle :: MonadIO m => Text -> Options -> m ()
handle callerName (Options (Just (Commands optAll))) = do
    mGame <- ifM (doesGameExist &&^ return (not optAll)) (Just <$> readGame) (return Nothing)

    exitWith success
        { messages = map (privateMessage callerName) (commandsMessages callerName mGame)
        }
handle callerName (Options (Just (Roles optAll))) = do
    roles <- (sortBy (compare `on` view Role.name) . nub) <$> ifM (doesGameExist &&^ return (not optAll))
        (toListOf (players . roles) <$> readGame)
        (return allRoles)

    exitWith success
        { messages = map (privateMessage callerName . roleMessage) roles
        }
handle callerName (Options (Just (Rules optAll))) = do
    mGame <- ifM (doesGameExist &&^ return (not optAll)) (Just <$> readGame) (return Nothing)

    exitWith success
        { messages = map (privateMessage callerName) (rulesMessages mGame)
        }
handle callerName (Options Nothing) = exitWith success
    { messages = map (privateMessage callerName) helpMessages
    }

commandsMessages :: Text -> Maybe Game -> [Text]
commandsMessages callerName mGame = map (T.intercalate "\n") $ filter (/= [])
    [ [ "Global commands:"
      , "- `start ([-e | --extra-roles ROLE,...] | [-r | --random-extra-roles]) PLAYER...`"
      , "- `end`"
      , "- `boot PLAYER`"
      , "- `quit`"
      , "- `version`"
      ]
    , [ "Status commands:"
      , "- `ping` ping the status of the current game publicly"
      , "- `status` get the status of the current game"
      , "- `circle [-a | --include-dead]` get the game circle"
      ]
    , [ "Standard commands:"
      , "- `vote PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame defenderRole
      [ "Defender commands:"
      , "- `protect PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame devotedServantRole
      [ "Devoted Servant commands:"
      , "- `reveal`"
      , "- `pass`"
      ]
    , whenPlayerHasRole callerName mGame scapegoatRole
      [ "Scapegoat commands:"
      , "- `choose PLAYER...`"
      ]
    , whenPlayerHasRole callerName mGame seerRole
      [ "Seer commands:"
      , "- `see PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame wildChildRole
      [ "Wild-child commands:"
      , "- `choose PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame witchRole
      [ "Witch commands:"
      , "- `heal`"
      , "- `poison PLAYER`"
      , "- `pass`"
      ]
    , whenPlayerHasRole callerName mGame wolfHoundRole
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

rulesMessages :: Maybe Game -> [Text]
rulesMessages mGame = map (T.intercalate "\n")
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
    , filter (/= "") [ T.concat
        [ "Each player is informed of their role (see `help roles' for a list)"
        , " at the start of the game."
        , " A game begins at night and follows a standard cycle."
        , whenRoleInPlay mGame angelRole
          " (N.B., when the Angel is in play the game begins with the village vote.)"
        ]
      , whenRoleInPlay mGame angelRole
        "- (When the Angel is in play) the village votes to lynch a suspect."
      , "- The village falls asleep."
      , whenRoleInPlay mGame wildChildRole
        "- (First round only) the Wild-child wakes up and chooses a role model."
      , whenRoleInPlay mGame defenderRole
        "- The Defender wakes up and protects someone."
      , whenRoleInPlay mGame seerRole
        "- The Seer wakes up and sees someone's allegiance."
      , whenRoleInPlay mGame wolfHoundRole
        "- (First round only) the Wolf-hound wakes up and chooses an allegiance."
      , "- The Werewolves wake up and select a victim."
      , whenRoleInPlay mGame witchRole
        "- The Witch wakes up and may heal the victim and/or poison someone."
      , "- The village wakes up and find the victim."
      , "- The village votes to lynch a suspect."
      , whenRoleInPlay mGame devotedServantRole
        "- The Devoted Servant may choose whether to reveal themselves and take on the role of their master."
      , whenRoleInPlay mGame scapegoatRole
        "- (When the Scapegoat is blamed) the Scapegoat chooses whom may vote on the next day."
      , T.concat
        [ "The game is over when only Villagers or Werewolves are left alive"
        , ifRoleInPlay mGame angelRole
          ", or when one of the Loners completes their own objective."
          "."
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
      , "- `help commands [-a | --all]`"
      , "- `help roles [-a | --all]`"
      , "- `help rules [-a | --all]`"
      ]
    ]

whenPlayerHasRole :: Monoid m => Text -> Maybe Game -> Role -> m -> m
whenPlayerHasRole _ Nothing _ m                   = m
whenPlayerHasRole callerName (Just game) role' m
     | has (players . names . only callerName) game
        && has (role . only role') player           = m
    | otherwise                                     = mempty
     where
        player = game ^?! players . traverse . filteredBy name callerName

ifRoleInPlay :: Maybe Game -> Role -> a -> a -> a
ifRoleInPlay Nothing _ true _               = true
ifRoleInPlay (Just game) role' true false
    | has (players . roles . only role') game   = true
    | otherwise                                 = false

whenRoleInPlay :: Monoid m => Maybe Game -> Role -> m -> m
whenRoleInPlay mGame role m = ifRoleInPlay mGame role m mempty
