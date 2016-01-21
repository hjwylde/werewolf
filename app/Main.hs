{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main, handle,
) where

import qualified Data.Text as T
import Data.Text (Text)

import Options.Applicative

import System.Environment

import qualified Werewolf.Commands.End       as End
import qualified Werewolf.Commands.Help      as Help
import qualified Werewolf.Commands.Interpret as Interpret
import qualified Werewolf.Commands.Quit      as Quit
import qualified Werewolf.Commands.See       as See
import qualified Werewolf.Commands.Start     as Start
import qualified Werewolf.Commands.Vote      as Vote
import           Werewolf.Options

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args = handleParseResult (execParserPure werewolfPrefs werewolfInfo args) >>= handle

interpret :: Text -> [Text] -> IO ()
interpret callerName args = do
    let result = execParserPure werewolfPrefs werewolfInfo (map T.unpack $ "--caller":callerName:args)

    case result of
        Success options -> handle options
        _               -> handle (Options callerName . Help . Help.Options $ Just Help.Commands)

handle :: Options -> IO ()
handle (Options callerName command) = case command of
    End                                 -> End.handle callerName
    Help options                        -> Help.handle callerName options
    Interpret (Interpret.Options args)  -> interpret callerName args
    Quit                                -> Quit.handle callerName
    See options                         -> See.handle callerName options
    Start options                       -> Start.handle callerName options
    Vote options                        -> Vote.handle callerName options
