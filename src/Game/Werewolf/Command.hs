{-|
Module      : Game.Werewolf.Command
Description : Command data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Command data structures.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Game.Werewolf.Command (
    -- * Command
    Command(..),
) where

import Game.Werewolf.Player

data Command = Vote
    { voter  :: Player
    , target :: Player
    } deriving (Eq, Show)
