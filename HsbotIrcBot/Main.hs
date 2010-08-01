module Main (main) where

import Control.Monad (when)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Hsbot.Irc.CLI
import Hsbot.Irc.Config

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
    -- From there the initialization code truly begins
    when (optDebug opts) . putStrLn $ "[hsbot-irc] Got CLI options :\n" ++ (show opts)
    -- We find and parse the config file
    ircConfig <- getIrcConfig $ optConfigFile opts
    when (optDebug opts) . putStrLn $ "[hsbot-irc] Compiled config :\n" ++ (show ircConfig)

