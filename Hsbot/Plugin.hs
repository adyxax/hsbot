module Hsbot.Plugin
    ( killPlugin
    , spawnPlugins
    , spawnPlugin
    , unloadPlugin
    ) where

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M
import Prelude hiding (catch)

import Hsbot.Config
import Hsbot.Irc.Config
import Hsbot.Irc.Core
import Hsbot.Types

-- | spawns plugins
spawnPlugins :: Bot ()
spawnPlugins = do
    config <- gets botConfig
    mapM_ (spawnPlugin) config

-- | spawns a single plugin
spawnPlugin :: BotConfig -> Bot ()
spawnPlugin (IrcBotConfig ircConfig) = do
    bot <- get
    let chan  = botChan bot
    pchan <- liftIO (newChan :: IO (Chan BotMsg))
    threadId <- liftIO $ forkIO (startIrcbot ircConfig chan pchan)
    let plugin  = PluginState { pluginName    = ircConfigName ircConfig
                              , pluginChan    = pchan
                              , pluginHandles = M.empty }
        plugins = botPlugins bot
    put $ bot { botPlugins = M.insert (pluginName plugin) (plugin, threadId) plugins }
    resumeData <- gets botResumeData
    liftIO $ modifyMVar_ resumeData (\oldData -> return $ M.insert (ircConfigName ircConfig) M.empty oldData)

-- | Unloads a plugin
unloadPlugin :: String -> Bot ()
unloadPlugin name = do
    killPlugin name
    resumeData <- gets botResumeData
    liftIO $ modifyMVar_ resumeData (\oldData -> return $ M.delete name oldData)

-- | kills a plugin
killPlugin :: String -> Bot ()
killPlugin name = do
    bot <- get
    let oldPlugins = botPlugins bot
    -- We check if the plugin exists
    case M.lookup name oldPlugins of
        Just (_, threadId) -> do
            let newPlugins = M.delete name oldPlugins
            liftIO $ throwTo threadId UserInterrupt
            put $ bot { botPlugins = newPlugins }
        Nothing            -> return ()

