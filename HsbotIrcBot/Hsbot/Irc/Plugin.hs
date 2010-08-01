module Hsbot.Irc.Plugin
    ( IrcPlugin
    , IrcPluginState (..)
    , killIrcPlugin
    , listPlugins
    , loadIrcPlugin
    , sendToPlugin
    , spawnIrcPlugins
    , unloadIrcPlugin
    ) where

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M

import Hsbot.Irc.Config
import Hsbot.Irc.Message
import Hsbot.Irc.Plugin.Core
import Hsbot.Irc.Plugin.Dummy
import Hsbot.Irc.Plugin.Ping
import Hsbot.Irc.Plugin.Quote
import Hsbot.Irc.Plugin.Utils
import Hsbot.Irc.Types

-- | Sends a msg to a plugin
sendToPlugin :: IrcBotMsg -> IrcPluginState -> IrcBot ()
sendToPlugin ircBotMsg plugin = do
    liftIO $ writeChan (ircPluginChan plugin) ircBotMsg

-- | spawns IrcPlugins
spawnIrcPlugins :: IrcBot ()
spawnIrcPlugins = do
    config <- gets ircBotConfig
    mapM_ (loadIrcPlugin) (ircConfigPlugins config)

-- | loads an ircbot plugin
loadIrcPlugin :: String -> IrcBot ()
loadIrcPlugin pluginName = do
    ircbot <- get
    let masterChan  = ircBotChan ircbot
    pluginChan <- liftIO (newChan :: IO (Chan IrcBotMsg))
    let entryPoint = case pluginName of
                        "Core"  -> ircBotPluginCore
                        "Ping"  -> ircBotPluginPing
                        "Quote" -> ircBotPluginQuote
                        _       -> ircBotPluginDummy
    let oldPlugins    = ircBotPlugins ircbot
    -- We check for unicity
    case M.lookup pluginName oldPlugins of
        Just _  -> return ()
        Nothing -> do
            mvar <- liftIO newEmptyMVar
            threadId <- liftIO . forkIO $ finally (entryPoint pluginChan masterChan) (putMVar mvar ())
            let plugin        = IrcPluginState { ircPluginName       = pluginName
                                               , ircPluginChan       = pluginChan
                                               , ircPluginMasterChan = masterChan }
                newPlugins    = M.insert pluginName (plugin, mvar, threadId) oldPlugins
            put $ ircbot { ircBotPlugins    = newPlugins }

-- | Sends a list of loaded plugins
listPlugins :: IrcMsg -> String -> IrcBot ()
listPlugins originalRequest dest = do
    plugins <- gets ircBotPlugins
    let listing = unwords $ M.keys plugins
    case M.lookup dest plugins of
        Just (plugin, _, _) -> sendToPlugin (IntIrcCmd $ IrcCmd "ANSWER" "CORE" dest listing originalRequest) plugin
        Nothing          -> return ()

-- | Unloads a plugin
unloadIrcPlugin :: String -> IrcBot ()
unloadIrcPlugin name = killIrcPlugin name

-- | kills a plugin
killIrcPlugin :: String -> IrcBot ()
killIrcPlugin name = do
    ircbot <- get
    let oldPlugins = ircBotPlugins ircbot
    -- We check if the plugin exists
    case M.lookup name oldPlugins of
        Just (_, mvar, threadId) -> do
            let newPlugins = M.delete name oldPlugins
            liftIO $ throwTo threadId UserInterrupt
            put $ ircbot { ircBotPlugins = newPlugins }
            liftIO $ takeMVar mvar
        Nothing            -> return ()

