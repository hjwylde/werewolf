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

import qualified Werewolf.Command.Boot      as Boot
import qualified Werewolf.Command.Choose    as Choose
import qualified Werewolf.Command.Circle    as Circle
import qualified Werewolf.Command.End       as End
import qualified Werewolf.Command.Heal      as Heal
import qualified Werewolf.Command.Help      as Help
import qualified Werewolf.Command.Interpret as Interpret
import qualified Werewolf.Command.Pass      as Pass
import qualified Werewolf.Command.Ping      as Ping
import qualified Werewolf.Command.Poison    as Poison
import qualified Werewolf.Command.Protect   as Protect
import qualified Werewolf.Command.Quit      as Quit
import qualified Werewolf.Command.See       as See
import qualified Werewolf.Command.Start     as Start
import qualified Werewolf.Command.Status    as Status
import qualified Werewolf.Command.Version   as Version
import qualified Werewolf.Command.Vote      as Vote
import           Werewolf.Options

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args = handleParseResult (execParserPure werewolfPrefs werewolfInfo args) >>= handle

interpret :: Text -> Text -> [Text] -> IO ()
interpret callerName tag args = do
    let result = execParserPure werewolfPrefs werewolfInfo (map T.unpack $ "--caller":callerName:"--tag":tag:args)

    case result of
        Success options -> handle options
        _               -> handle (Options callerName tag . Help . Help.Options . Just $ Help.Commands False)

handle :: Options -> IO ()
handle (Options callerName tag command) = case command of
    Choose options                      -> Choose.handle callerName tag options
    Boot options                        -> Boot.handle callerName tag options
    Circle options                      -> Circle.handle callerName tag options
    End options                         -> End.handle callerName tag options
    Heal                                -> Heal.handle callerName tag
    Help options                        -> Help.handle callerName tag options
    Interpret (Interpret.Options args)  -> interpret callerName tag args
    Pass                                -> Pass.handle callerName tag
    Ping                                -> Ping.handle callerName tag
    Poison options                      -> Poison.handle callerName tag options
    Protect options                     -> Protect.handle callerName tag options
    Quit                                -> Quit.handle callerName tag
    See options                         -> See.handle callerName tag options
    Start options                       -> Start.handle callerName tag options
    Status                              -> Status.handle callerName tag
    Version                             -> Version.handle callerName
    Vote options                        -> Vote.handle callerName tag options
