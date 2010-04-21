module Hsbot.Plugin
    ( listPlugins
    , loadPlugin
    , sendToPlugin
    , unloadPlugin
    ) where

import Control.Concurrent
import Control.Concurrent.Chan()
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe()
import System.IO()

import Hsbot.Types
import Hsbot.Utils

-- | Loads a plugin into an ircBot
loadPlugin :: String -> (Chan BotMsg -> Chan BotMsg -> IO ()) -> IrcBot ()
loadPlugin name entryPoint = do
    bot <- get
    let oldPlugins = botPlugins bot
    plugin <- liftIO $ effectivelyLoadPlugin name entryPoint (botChannel bot)
    put $ bot { botPlugins = M.insert name plugin oldPlugins}

-- | Effectively try to load a plugin
effectivelyLoadPlugin :: String -> (Chan BotMsg -> Chan BotMsg -> IO ()) -> Chan BotMsg -> IO (Plugin)
effectivelyLoadPlugin name entryPoint serverChan = do
    putStrLn $ inColor ("Loaded (static) plugin: " ++ name) [32]
    chan <- newChan :: IO (Chan BotMsg)
    threadId <- forkIO $ entryPoint serverChan chan
    return $ Plugin name threadId chan

-- | Sends a list of loaded plugins
listPlugins :: Maybe IrcMsg -> String -> IrcBot ()
listPlugins originalRequest dest = do
    plugins <- gets botPlugins
    let listing = unwords $ M.keys plugins
    case M.lookup dest plugins of
        Just plugin -> sendToPlugin (InternalCmd $ IntCmd "ANSWER" "CORE" dest listing originalRequest) plugin
        Nothing     -> return ()

-- | Unloads a plugin
unloadPlugin :: String -> IrcBot ()
unloadPlugin name = do
    bot <- get
    let oldPlugins = botPlugins bot
    case M.lookup name oldPlugins of
        Just plugin -> do
            let newPlugins = M.delete name oldPlugins
            liftIO $ throwTo (pluginThreadId plugin) UserInterrupt
            put $ bot { botPlugins = newPlugins }
        Nothing     -> return ()

-- | Sends a msg to a plugin
sendToPlugin :: BotMsg -> Plugin -> IrcBot ()
sendToPlugin msg plugin = do
    let chan = pluginChannel plugin
    liftIO $ writeChan chan msg

