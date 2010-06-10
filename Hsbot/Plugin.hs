module Hsbot.Plugin
    ( killPlugin
    , spawnPlugins
    , spawnPlugin
    , unloadPlugin
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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
    let mvar = botResumeData bot
        name = ircConfigName ircConfig
    resumeData <- liftIO $ takeMVar mvar
    let pluginResumeData = fromMaybe M.empty $ M.lookup name resumeData
        chan = botChan bot
    pchan <- liftIO (newChan :: IO (Chan BotMsg))
    pluginMVar <- liftIO newEmptyMVar
    threadId <- liftIO . forkIO $ finally (startIrcbot ircConfig chan pchan (Just $ show pluginResumeData)) (putMVar pluginMVar ())
    let plugin  = PluginState { pluginName    = name
                              , pluginChan    = pchan
                              , pluginHandles = M.empty }
        plugins = botPlugins bot
    put $ bot { botPlugins = M.insert (pluginName plugin) (plugin, pluginMVar, threadId) plugins }
    liftIO . putMVar mvar $ M.insert name pluginResumeData resumeData

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
        Just (_, mvar, threadId) -> do
            let newPlugins = M.delete name oldPlugins
            liftIO $ throwTo threadId UserInterrupt
            put $ bot { botPlugins = newPlugins }
            liftIO $ takeMVar mvar
        Nothing            -> return ()

