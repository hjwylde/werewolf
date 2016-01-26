{-|
Module      : Game.Werewolf.Test.Player
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Game.Werewolf.Test.Player (
    prop_newPlayerIsAlive,
) where

import Control.Lens

import Data.Text

import Game.Werewolf.Player
import Game.Werewolf.Role

prop_newPlayerIsAlive :: Text -> Role -> Bool
prop_newPlayerIsAlive name role = newPlayer name role ^. state == Alive
