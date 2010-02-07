module Hsbot.Plugin
    ( loadPlugin
    , sendToPlugin
    , reloadPlugin
    , unloadPlugin
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.Plugins

import Hsbot.Types
import Hsbot.Utils

-- TODO : reload plugin, list plugins, etc

-- | Loads a plugin into an ircBot
loadPlugin :: String -> IrcBot ()
loadPlugin name = do
    bot <- get
    let oldPlugins = botPlugins bot
    if name `M.member` oldPlugins
      then traceM $ inColor ("Can't load plugin \"" ++ name ++ "\", this identifier has already been registered.") [31] -- or a wait, smthg like that?
      else do
        plugin <- liftIO $ effectivelyLoadPlugin name (botChannel bot)
        case plugin of
            Just plugin' -> do
                put $ bot { botPlugins = M.insert name plugin' oldPlugins}
            Nothing -> return ()

-- | Effectively try to load a plugin
effectivelyLoadPlugin :: String -> Chan BotMsg -> IO (Maybe Plugin)
effectivelyLoadPlugin name serverChan = do
    -- TODO : test if Plugins/ ++ name ++ .hs exists
    m <- liftIO $ makeAll ("Plugins/" ++ name ++ ".hs") ["-XScopedTypeVariables"]
    plugin <- case m of
        MakeSuccess _ _ -> do
            ldstat <- load_ ("Plugins/" ++ name ++ ".o") [".","Hsbot","Hsbot/Plugins"] ("main" ++ name)
            case ldstat of
                LoadSuccess v entryPoint -> do
                    putStrLn $ inColor ("Loaded plugin: " ++ name) [32]
                    chan <- newChan :: IO (Chan BotMsg)
                    threadId <- forkIO $ entryPoint serverChan chan
                    return $ Just (Plugin name v threadId chan)
                LoadFailure e -> do
                    putStrLn $ inColor ("Couldn't load plugin: " ++ name) [31]
                    mapM_ putStrLn e
                    return Nothing
        MakeFailure e -> do
            putStrLn $ inColor ("FATAL: Couldn't compile plugin: " ++ name) [31]
            mapM_ putStrLn e
            return Nothing
    return plugin

-- | Reloads a plugin
-- TODO : make it a safe reload (compile before unloading)
reloadPlugin :: String -> IrcBot ()
reloadPlugin name = do
    unloadPlugin name
    loadPlugin name

-- | Unloads a plugin
unloadPlugin :: String -> IrcBot ()
unloadPlugin name = do
    bot <- get
    let oldPlugins = botPlugins bot
    case M.lookup name oldPlugins of
        Just plugin -> do
            let newPlugins = M.delete name oldPlugins
            liftIO $ throwTo (pluginThreadId plugin) UserInterrupt
            liftIO $ unloadAll $ pluginModule plugin
            put $ bot { botPlugins = newPlugins }
        Nothing -> return ()

-- | Sends a msg to a plugin
sendToPlugin :: BotMsg -> Plugin -> IrcBot ()
sendToPlugin msg plugin = do
    let chan = pluginChannel plugin
    liftIO $ writeChan chan msg

