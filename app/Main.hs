{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module Main (
    main,
) where

import Options.Applicative

import qualified Werewolf.Commands.End       as End
import qualified Werewolf.Commands.Help      as Help
import qualified Werewolf.Commands.Interpret as Interpret
import qualified Werewolf.Commands.Start     as Start
import qualified Werewolf.Commands.Vote      as Vote
import           Werewolf.Options

main :: IO ()
main = customExecParser werewolfPrefs werewolfInfo >>= handle

handle :: Options -> IO ()
handle (Options caller command) = case command of
    End                 -> End.handle caller
    Help options        -> Help.handle caller options
    Interpret options   -> Interpret.handle caller options
    Start options       -> Start.handle caller options
    Vote options        -> Vote.handle caller options
