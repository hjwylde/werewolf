{-|
Module      : Game.Werewolf.Variant.NoRoleKnowledge.Engine
Description : Suite of engine messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of engine messages used throughout the werewolf game for the 'NoRoleKnowledge'
variant.
-}

{-# LANGUAGE QuasiQuotes #-}

module Game.Werewolf.Variant.NoRoleKnowledge.Engine (
    -- * New game
    rolesInGameText,
) where

import Control.Lens

import           Data.String.Interpolate.Extra
import           Data.Text                     (Text)
import qualified Data.Text                     as T

import Game.Werewolf

rolesInGameText :: Game -> Text
rolesInGameText game = [iFile|variant/no-role-knowledge/engine/new-game/roles-in-game.txt|]
    where
        totalBalance = sumOf (players . roles . balance) game
