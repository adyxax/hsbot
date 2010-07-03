module Main (main) where

import Control.Monad (when)
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Info
import System.IO
import System.Posix.Process (executeFile)
import System.Process

-- | Dynamic launching function
main :: IO ()
main = do
    args <- getArgs
    case args of
        []         -> buildLaunch
        ["--help"] -> usage
        _          -> fail "unrecognized flags"

usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        concat ["Usage: ", self, " [OPTION]"] :
        "Options:" :
        "  --help : Print this message" :
        []

buildLaunch ::  IO ()
buildLaunch = do
    _ <- recompile
    dir  <- getAppUserDataDirectory "hsbot"
    args <- getArgs
    _ <- executeFile (dir ++ "/hsbot-" ++ arch ++ "-" ++ os) False args Nothing
    return ()

recompile :: IO (Bool)
recompile = do
    dir <- getAppUserDataDirectory "hsbot"
    let binn = "hsbot-"++arch++"-"++os
        base = dir </> "hsbot"
        err  = base ++ ".errors"
        src  = base ++ ".hs"
    errorHandle <- openFile err WriteMode
    exitCode <- waitForProcess =<< runProcess "ghc" ["--make", "hsbot.hs", "-fforce-recomp", "-XScopedTypeVariables", "-o", binn] (Just dir)
                                              Nothing Nothing Nothing (Just errorHandle)
    hClose errorHandle
    when (exitCode /= ExitSuccess) $ do
        ghcErr <- readFile err
        let msg = unlines $
                ["Error detected while loading hsbot configuration file: " ++ src]
                ++ lines ghcErr ++ ["","Please check the file for errors."]
        hPutStrLn stderr msg
    return (exitCode == ExitSuccess)

