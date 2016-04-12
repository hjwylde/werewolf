{-|
Module      : Game.Werewolf.Test.Util
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Util (
    -- * Utility functions
    run, run_, verbose_runCommandErrors,
) where

import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State  hiding (State)
import Control.Monad.Writer

import Data.Either.Extra

import Game.Werewolf

import Test.QuickCheck

run :: StateT Game (WriterT [Message] (ExceptT [Message] (Rand StdGen))) a -> Game -> Either [Message] (Game, [Message])
run action game = evalRand (runExceptT . runWriterT $ execStateT action game) (mkStdGen 0)

run_ :: StateT Game (WriterT [Message] (ExceptT [Message] (Rand StdGen))) a -> Game -> Game
run_ action = fst . fromRight . run action

verbose_runCommandErrors :: Game -> Command -> Property
verbose_runCommandErrors game command = whenFail (mapM_ putStrLn data_) (isLeft result)
    where
        result  = run (apply command) game
        data_   = [show game, show $ fromRight result]
