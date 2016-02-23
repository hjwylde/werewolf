{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (
    -- * Main
    main,
) where

import           Data.Text (Text)
import qualified Data.Text as T

import Options.Applicative

import System.Environment

import qualified Werewolf.Commands.Choose    as Choose
import qualified Werewolf.Commands.Circle    as Circle
import qualified Werewolf.Commands.End       as End
import qualified Werewolf.Commands.Heal      as Heal
import qualified Werewolf.Commands.Help      as Help
import qualified Werewolf.Commands.Interpret as Interpret
import qualified Werewolf.Commands.Pass      as Pass
import qualified Werewolf.Commands.Ping      as Ping
import qualified Werewolf.Commands.Poison    as Poison
import qualified Werewolf.Commands.Protect   as Protect
import qualified Werewolf.Commands.Quit      as Quit
import qualified Werewolf.Commands.See       as See
import qualified Werewolf.Commands.Start     as Start
import qualified Werewolf.Commands.Status    as Status
import qualified Werewolf.Commands.Version   as Version
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
    Choose options                      -> Choose.handle callerName options
    Circle options                      -> Circle.handle callerName options
    End                                 -> End.handle callerName
    Heal                                -> Heal.handle callerName
    Help options                        -> Help.handle callerName options
    Interpret (Interpret.Options args)  -> interpret callerName args
    Pass                                -> Pass.handle callerName
    Ping                                -> Ping.handle callerName
    Poison options                      -> Poison.handle callerName options
    Protect options                     -> Protect.handle callerName options
    Quit                                -> Quit.handle callerName
    See options                         -> See.handle callerName options
    Start options                       -> Start.handle callerName options
    Status                              -> Status.handle callerName
    Version                             -> Version.handle callerName
    Vote options                        -> Vote.handle callerName options
