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

handle :: MonadIO m => Text -> Text -> Options -> m ()
handle callerName tag (Options (Just (Commands optAll))) = do
    mGame <- ifM (doesGameExist tag &&^ return (not optAll)) (Just <$> readGame tag) (return Nothing)

    exitWith success
        { messages = map (privateMessage callerName) (commandsMessages callerName mGame)
        }
handle callerName tag (Options (Just (Roles optAll))) = do
    roles <- (sortBy (compare `on` view Role.name) . nub) <$> ifM (doesGameExist tag &&^ return (not optAll))
        (toListOf (players . roles) <$> readGame tag)
        (return allRoles)

    exitWith success
        { messages = map (privateMessage callerName . roleMessage) roles
        }
handle callerName tag (Options (Just (Rules optAll))) = do
    mGame <- ifM (doesGameExist tag &&^ return (not optAll)) (Just <$> readGame tag) (return Nothing)

    exitWith success
        { messages = map (privateMessage callerName) (rulesMessages mGame)
        }
handle callerName _ (Options Nothing) = exitWith success
    { messages = map (privateMessage callerName) helpMessages
    }

commandsMessages :: Text -> Maybe Game -> [Text]
commandsMessages callerName mGame = map (T.intercalate "\n") $ filter (/= [])
    [ [ "Global commands:"
      , "- `start ([-e|--extra-roles ROLE,...] | [-r|--random-extra-roles]) [--include-seer] PLAYER...`"
      , "- `end`"
      , "- `boot PLAYER`"
      , "- `quit`"
      , "- `version`"
      ]
    , [ "Status commands:"
      , "- `ping` ping the status of the current game publicly"
      , "- `status` get the status of the current game privately"
      , "- `circle [-a|--include-dead]` get the game circle"
      ]
    , [ "Standard commands:"
      , "- `vote PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame hunterRole
      [ "Hunter commands:"
      , "- `choose PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame orphanRole
      [ "Orphan commands:"
      , "- `choose PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame protectorRole
      [ "Protector commands:"
      , "- `protect PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame scapegoatRole
      [ "Scapegoat commands:"
      , "- `choose PLAYER...`"
      ]
    , whenPlayerHasRole callerName mGame seerRole
      [ "Seer commands:"
      , "- `see PLAYER`"
      ]
    , whenPlayerHasRole callerName mGame witchRole
      [ "Witch commands:"
      , "- `heal`"
      , "- `poison PLAYER`"
      , "- `pass`"
      ]
    ]

roleMessage :: Role -> Text
roleMessage role = T.intercalate "\n"
    [ T.concat [role ^. Role.name, " (", T.pack . show $ role ^. balance, "):"]
    , role ^. description
    , role ^. rules
    ]

rulesMessages :: Maybe Game -> [Text]
rulesMessages mGame = map (T.intercalate "\n")
    [ [ T.unwords
        [ "Each player is informed of their role (see `help roles` for a list) at the start of the"
        , "game."
        ]
      , T.unwords
        [ "Each night, the Werewolves transform and subsequently assault and devour one Villager."
        , "After feasting, their lycanthropic form subsides and they once again hide in plain"
        , "sight."
        ]
      , T.unwords
        [ "Each day, after discovering the victim, the village gathers in the town square. In a"
        , "democratic fashion they then vote for whom they believe to be a Werewolf. The votee is"
        , "immediately tied to a pyre and burned alive in an attempt to rid Fougères of all"
        , "lupines."
        ]
      ]
    , filter (/= "")
      [ T.concat
        [ "A game begins at night and follows a standard cycle."
        , whenRoleInPlay mGame fallenAngelRole
            " (N.B., when the Fallen Angel is in play the game begins with the village vote.)"
        ]
      , whenRoleInPlay mGame fallenAngelRole
        "- (When the Fallen Angel is in play) the village votes to lynch a suspect."
      , "- The village falls asleep."
      , whenRoleInPlay mGame orphanRole
        "- (First round only) the Orphan wakes up and chooses a role model."
      , whenRoleInPlay mGame villageDrunkRole
        "- (Third round only) the Village Drunk sobers up and remembers their allegiance."
      , whenRoleInPlay mGame seerRole
        "- The Seer wakes up and sees someone's allegiance."
      , whenRoleInPlay mGame protectorRole
        "- The Protector wakes up and protects someone."
      , "- The Werewolves wake up and vote to devour a victim."
      , whenRoleInPlay mGame witchRole
        "- The Witch wakes up and may heal the victim and/or poison someone."
      , "- The village wakes up and find the victim."
      , whenRoleInPlay mGame hunterRole
        "- (When the Hunter is killed) the Hunter chooses someone to shoot."
      , whenRoleInPlay mGame druidRole
        "- Ferina grunts if the Druid is next to a Werewolf."
      , "- The village votes to lynch a suspect."
      , whenRoleInPlay mGame hunterRole
        "- (When the Hunter is killed) the Hunter chooses someone to shoot."
      , whenRoleInPlay mGame scapegoatRole
        "- (When the Scapegoat is blamed) the Scapegoat chooses whom may vote on the next day."
      , T.concat
        [ "The game is over when only Villagers or Werewolves are left alive"
        , ifRoleInPlay mGame fallenAngelRole
          ", or when one of the Loners completes their own objective."
          "."
        ]
      ]
    ]

helpMessages :: [Text]
helpMessages = map (T.intercalate "\n")
    [ [ T.unwords
        [ "Long has the woods been home to wild creatures, both kind and cruel. Most have faces and"
        , "are known to the inhabitants of Fougères in Brittany, France; but no-one from the"
        , "village has yet to lay eyes on the merciless Werewolf."
        ]
      , T.unwords
        [ "Each night Werewolves attack the village and devour the innocent. For centuries no-one"
        , "knew how to fight this scourge, however recently a theory has taken ahold that mayhaps"
        , "the Werewolves walk among the Villagers themselves..."
        ]
      ]
    , [ "Help commands:"
      , "- `help commands [-a | --all]`"
      , "- `help roles [-a | --all]`"
      , "- `help rules [-a | --all]`"
      ]
    ]

whenPlayerHasRole :: Monoid m => Text -> Maybe Game -> Role -> m -> m
whenPlayerHasRole _ Nothing _ m                         = m
whenPlayerHasRole callerName (Just game) role' m
    | hasn't (players . names . only callerName) game   = mempty
    | hasn't (role . only role') player                 = mempty
    | otherwise                                         = m
     where
        player = game ^?! players . traverse . filteredBy name callerName

ifRoleInPlay :: Maybe Game -> Role -> a -> a -> a
ifRoleInPlay Nothing _ true _               = true
ifRoleInPlay (Just game) role' true false
    | has (players . roles . only role') game   = true
    | otherwise                                 = false

whenRoleInPlay :: Monoid m => Maybe Game -> Role -> m -> m
whenRoleInPlay mGame role m = ifRoleInPlay mGame role m mempty
