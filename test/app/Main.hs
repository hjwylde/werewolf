{-|
Module      : Main
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Main (
    -- * Main
    main
) where

import Game.Werewolf.Test.Command
import Game.Werewolf.Test.Engine

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain . localOption (QuickCheckTests 20) =<< tests

tests :: IO TestTree
tests = return . testGroup "Tests" $ allCommandTests ++ allEngineTests
