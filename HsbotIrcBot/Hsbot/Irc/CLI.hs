module Hsbot.Irc.CLI
    ( Options (..)
    , defaultOptions
    , header
    , options
    ) where

import System.Console.GetOpt
import System.Exit

-- CLI argument parting stuff {{{
-- | CLI options
data Options = Options
    { optDebug      :: Bool
    , optConfigFile :: Maybe String
    , optGroup      :: Maybe String
    , optUser       :: Maybe String
    , optVerbose    :: Bool
    } deriving (Show)

-- | CLI default options
defaultOptions :: Options
defaultOptions = Options { optDebug      = False
                         , optConfigFile = Nothing
                         , optGroup      = Nothing
                         , optUser       = Nothing
                         , optVerbose    = False }

-- | CLI options logic
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "d" ["debug"]
        (NoArg (\opt -> return opt { optDebug = True, optVerbose = True }))
        "Enter verbose debug mode and prevents Hsbot from forking in background"
    , Option "f" ["file"]
        (ReqArg (\arg opt -> return opt { optConfigFile = return arg }) "<config_file>")
        "The config file to use"
    , Option "g" ["group"]
        (ReqArg (\arg opt -> return opt { optGroup = return arg }) "<group>")
        "The group hsbot will run as"
    , Option "h" ["help"]
        (NoArg (\_ -> do
                    putStrLn $ usageInfo header options
                    exitWith ExitSuccess))
        "Print this help message"
    , Option "u" ["user"]
        (ReqArg (\arg opt -> return opt { optUser = return arg }) "<user>")
        "The user hsbot will run as"
    , Option "v" ["verbose"]
        (NoArg (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
    , Option "V" ["version"]
        (NoArg (\_ -> do
                    putStrLn "hsbot-irc version 0.3"
                    exitWith ExitSuccess))
        "Show version"
    ]

-- | Usage header
header :: String
header = "Usage: hsbot-irc [-dhvV] [-f config_file] [-u user] [-g group]"
-- }}}

