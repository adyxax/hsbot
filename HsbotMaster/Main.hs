module Main (main) where

import Control.Monad (when)
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

-- | Main function
main :: IO ()
main = do
    args <- getArgs
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    -- Here we thread startOptions through all supplied option actions
    opts <- case (nonOptions, errors) of
        ([], []) -> foldl (>>=) (return defaultOptions) actions
        (_, _) -> do
            hPutStrLn stderr $ concat errors ++ usageInfo header options
            exitWith $ ExitFailure 1
    when (optDebug opts) . putStrLn $ "Got options : " ++ (show opts)

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
                    putStrLn "Hsbot version 0.3"
                    exitWith ExitSuccess))
        "Show version"
    ]

-- | Usage header
header :: String
header = "Usage: hsbot [-dhvV] [-f config_file] [-u user] [-g group]"

