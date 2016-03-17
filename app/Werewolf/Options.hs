{-|
Module      : Werewolf.Options
Description : Optparse utilities.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Optparse utilities.
-}

{-# LANGUAGE OverloadedStrings #-}

module Werewolf.Options (
    -- * Options
    Options(..), Command(..),

    -- * Optparse
    werewolfPrefs, werewolfInfo, werewolf,
) where

import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Version (showVersion)

import qualified Werewolf.Command.Choose    as Choose
import qualified Werewolf.Command.Circle    as Circle
import qualified Werewolf.Command.End as End
import qualified Werewolf.Command.Help      as Help
import qualified Werewolf.Command.Interpret as Interpret
import qualified Werewolf.Command.Poison    as Poison
import qualified Werewolf.Command.Protect   as Protect
import qualified Werewolf.Command.See       as See
import qualified Werewolf.Command.Start     as Start
import qualified Werewolf.Command.Vote      as Vote
import qualified Werewolf.Version           as This

import Options.Applicative

data Options = Options
    { optCaller  :: Text
    , argCommand :: Command
    } deriving (Eq, Show)

data Command
    = Choose Choose.Options
    | Circle Circle.Options
    | End End.Options
    | Heal
    | Help Help.Options
    | Interpret Interpret.Options
    | Pass
    | Ping
    | Poison Poison.Options
    | Protect Protect.Options
    | Quit
    | Reveal
    | See See.Options
    | Start Start.Options
    | Status
    | Version
    | Vote Vote.Options
    deriving (Eq, Show)

-- | The default preferences.
--   Limits the help output to 100 columns.
werewolfPrefs :: ParserPrefs
werewolfPrefs = prefs $ columns 100

-- | An optparse parser of a werewolf command.
werewolfInfo :: ParserInfo Options
werewolfInfo = info (infoOptions <*> werewolf) (fullDesc <> header' <> progDesc')
    where
        infoOptions = helper <*> version
        version     = infoOption ("Version " ++ showVersion This.version) $ mconcat
            [ long "version", short 'V', hidden
            , help "Show this binary's version"
            ]

        header'     = header "A game engine for running werewolf in a chat client."
        progDesc'   = progDesc $ unwords
            [ "This engine is based off of Werewolves of Millers Hollow"
            , "(http://www.games-wiki.org/wiki/Werewolves_of_Millers_Hollow/)."
            , "See https://github.com/hjwylde/werewolf for help on writing chat interfaces."
            ]

-- | An options parser.
werewolf :: Parser Options
werewolf = Options
    <$> fmap T.pack (strOption $ mconcat
        [ long "caller", metavar "PLAYER"
        , help "Specify the calling player's name"
        ])
    <*> subparser (mconcat
        [ command "choose"      $ info (helper <*> choose)      (fullDesc <> progDesc "Choose an allegiance or player(s)")
        , command "circle"      $ info (helper <*> circle)      (fullDesc <> progDesc "Get the game circle")
        , command "end"         $ info (helper <*> end)         (fullDesc <> progDesc "End the current game")
        , command "heal"        $ info (helper <*> heal)        (fullDesc <> progDesc "Heal the devoured player")
        , command "help"        $ info (helper <*> help_)       (fullDesc <> progDesc "Help documents")
        , command "interpret"   $ info (helper <*> interpret)   (fullDesc <> noIntersperse <> progDesc "Interpret a command")
        , command "pass"        $ info (helper <*> pass)        (fullDesc <> progDesc "Pass")
        , command "ping"        $ info (helper <*> ping)        (fullDesc <> progDesc "Ping the status of the current game publicly")
        , command "poison"      $ info (helper <*> poison)      (fullDesc <> progDesc "Poison a player")
        , command "protect"     $ info (helper <*> protect)     (fullDesc <> progDesc "Protect a player")
        , command "quit"        $ info (helper <*> quit)        (fullDesc <> progDesc "Quit the current game")
        , command "reveal"      $ info (helper <*> reveal)      (fullDesc <> progDesc "Reveal yourself")
        , command "see"         $ info (helper <*> see)         (fullDesc <> progDesc "See a player's allegiance")
        , command "start"       $ info (helper <*> start)       (fullDesc <> progDesc "Start a new game")
        , command "status"      $ info (helper <*> status)      (fullDesc <> progDesc "Get the status of the current game")
        , command "version"     $ info (helper <*> version)     (fullDesc <> progDesc "Show this engine's version")
        , command "vote"        $ info (helper <*> vote)        (fullDesc <> progDesc "Vote against a player")
        ])

choose :: Parser Command
choose = Choose . Choose.Options . map T.pack <$> some (strArgument $ metavar "(ALLEGIANCE | PLAYER...)")

circle :: Parser Command
circle = Circle . Circle.Options
    <$> switch (mconcat
        [ long "include-dead", short 'a'
        , help "Include dead players"
        ])

end :: Parser Command
end = End . End.Options
    <$> switch (mconcat
        [ long "force", short 'f'
        , help "Force the game to end"
        ])

heal :: Parser Command
heal = pure Heal

help_ :: Parser Command
help_ = Help . Help.Options
    <$> optional (subparser $ mconcat
        [ command "commands"    $ info (Help.Commands <$> allOption)    (fullDesc <> progDesc "Print the in-game commands")
        , command "roles"       $ info (Help.Roles <$> allOption)       (fullDesc <> progDesc "Print the roles and their descriptions")
        , command "rules"       $ info (Help.Rules <$> allOption)       (fullDesc <> progDesc "Print the game rules")
        ])
    where
        allOption = switch $ long "all" <> short 'a'

interpret :: Parser Command
interpret = Interpret . Interpret.Options
    <$> many (T.pack <$> strArgument (metavar "-- COMMAND ARG..."))

pass :: Parser Command
pass = pure Pass

ping :: Parser Command
ping = pure Ping

poison :: Parser Command
poison = Poison . Poison.Options <$> playerArgument

protect :: Parser Command
protect = Protect . Protect.Options <$> playerArgument

quit :: Parser Command
quit = pure Quit

reveal :: Parser Command
reveal = pure Reveal

see :: Parser Command
see = See . See.Options <$> playerArgument

start :: Parser Command
start = fmap Start $ Start.Options
    <$> (extraRolesOption <|> randomExtraRolesOption)
    <*> some (T.pack <$> strArgument (metavar "PLAYER..."))
    where
        extraRolesOption = fmap (Start.Use . filter (/= T.empty) . T.splitOn "," . T.pack) (strOption $ mconcat
            [ long "extra-roles", short 'e', metavar "ROLE,..."
            , value []
            , help "Specify the extra roles to use"
            ])

        randomExtraRolesOption = flag Start.None Start.Random $ mconcat
            [ long "random-extra-roles", short 'r'
            , help "Use random extra roles"
            ]

status :: Parser Command
status = pure Status

version :: Parser Command
version = pure Version

vote :: Parser Command
vote = Vote . Vote.Options <$> playerArgument

playerArgument :: Parser Text
playerArgument = T.pack <$> strArgument (metavar "PLAYER")
