{-|
Module      : Main
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Main (
    -- * Main
    main
) where

import Game.Werewolf.Test.Command
import Game.Werewolf.Test.Engine
import Game.Werewolf.Test.Game
import Game.Werewolf.Test.Player

import Test.Tasty

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = return . testGroup "Tests" $ concat
    [allCommandTests, allEngineTests, allGameTests, allPlayerTests]
