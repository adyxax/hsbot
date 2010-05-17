module Hsbot.Irc.Plugin
    ( IrcPlugin
    , IrcPluginState (..)
    , listPlugins
    , loadIrcPlugin
    , sendToPlugin
    , spawnIrcPlugins
    , unloadPlugin
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
    let oldPlugins = ircBotPlugins ircbot
    -- We check for unicity
    case M.lookup pluginName oldPlugins of
        Just _  -> return ()
        Nothing -> do
            threadId <- liftIO $ forkIO (entryPoint pluginChan masterChan)
            let plugin  = IrcPluginState { ircPluginName       = pluginName
                                         , ircPluginChan       = pluginChan
                                         , ircPluginMasterChan = masterChan }
            put $ ircbot { ircBotPlugins = M.insert pluginName (plugin, threadId) oldPlugins }

-- | Sends a list of loaded plugins
listPlugins :: IrcMsg -> String -> IrcBot ()
listPlugins originalRequest dest = do
    plugins <- gets ircBotPlugins
    let listing = unwords $ M.keys plugins
    case M.lookup dest plugins of
        Just (plugin, _) -> sendToPlugin (IntIrcCmd $ IrcCmd "ANSWER" "CORE" dest listing originalRequest) plugin
        Nothing          -> return ()

-- | Unloads a plugin
unloadPlugin :: String -> IrcBot ()
unloadPlugin name = do
    bot <- get
    let oldPlugins = ircBotPlugins bot
    case M.lookup name oldPlugins of
        Just (_, threadId) -> do
            let newPlugins = M.delete name oldPlugins
            liftIO $ throwTo threadId UserInterrupt
            put $ bot { ircBotPlugins = newPlugins }
        Nothing     -> return ()

