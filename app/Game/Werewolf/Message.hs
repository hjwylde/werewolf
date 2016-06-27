{-|
Module      : Game.Werewolf.Message
Description : Utility functions for building messages.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines some utility functions for building messages. See "Game.Werewolf.Message.Command",
"Game.Werewolf.Message.Engine" and "Game.Werewolf.Message.Error" for actual message definitions.

@werewolf@ was designed to be ambivalent to the playing chat client. The response-message structure
reflects this by staying away from anything that could be construed as client-specific. This
includes features such as emoji support.
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Message (
    -- * Utility functions
    humanisePlayerWithRole, humanisePlayersWithRoles, humanisePlayerWithRoleIfKnown,
    humanisePlayerWithState, article, conjugateToBe, pluralise,
) where

import Control.Lens.Extra

import           Data.String.Humanise
import           Data.Text            (Text)
import qualified Data.Text            as T

import Game.Werewolf.Player
import Game.Werewolf.Role   hiding (name)

-- TODO (hjw): tidy up all of the messages

humanisePlayerWithRole :: Player -> Text
humanisePlayerWithRole player = T.concat [humanise player, " (", humanise $ player ^. role, ")"]

humanisePlayersWithRoles :: [Player] -> Text
humanisePlayersWithRoles = humanise . map humanisePlayerWithRole

humanisePlayerWithRoleIfKnown :: Player -> Text
humanisePlayerWithRoleIfKnown player
    | any ($ player) [is trueVillager, is zombie]   = humanisePlayerWithRole player
    | otherwise                                     = humanise player

humanisePlayerWithState :: Player -> Text
humanisePlayerWithState player
    | is alive player   = humanise player
    | otherwise         = T.concat [humanise player, " (dead)"]

article :: Role -> Text
article role
    | role `elem` restrictedRoles   = "the"
    | otherwise                     = "a"

conjugateToBe :: Int -> Text
conjugateToBe 1 = "is"
conjugateToBe _ = "are"

pluralise :: Int -> Text -> Text
pluralise 1 word = word
pluralise _ word = T.snoc word 's'
