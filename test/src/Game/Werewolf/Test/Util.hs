{-|
Module      : Game.Werewolf.Test.Util
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Game.Werewolf.Test.Util (
    -- * Utility functions
    run, run_,
) where

import Control.Monad.Except
import Control.Monad.State  hiding (State)
import Control.Monad.Writer

import Data.Either.Extra

import Game.Werewolf.Game
import Game.Werewolf.Response

run :: StateT Game (WriterT [Message] (Except [Message])) a -> Game -> Either [Message] (Game, [Message])
run action game = runExcept . runWriterT $ execStateT action game

run_ :: StateT Game (WriterT [Message] (Except [Message])) a -> Game -> Game
run_ action = fst . fromRight . run action
