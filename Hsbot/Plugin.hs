module Hsbot.Plugin
    ( loadPlugin
    ) where

import Control.Concurrent
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import System.Log.Logger

import Hsbot.Types

loadPlugin :: PluginId -> Env IO ()
loadPlugin pId = do
    botMVar <- asks envBotState
    liftIO (takeMVar botMVar) >>= execStateT effectivelyLoadPlugin >>= liftIO . putMVar botMVar
  where
    effectivelyLoadPlugin :: Bot (Env IO) ()
    effectivelyLoadPlugin = do
        bot <- get
        chan <- liftIO (newChan :: IO (Chan Message))
        master <- lift $ asks envChan
        let name = pluginName pId
            loop = pluginEp pId
            oldPlugins = botPlugins bot
            pEnv = PluginEnv { pluginId     = pId
                             , pluginChan   = chan
                             , pluginMaster = master }
        case M.lookup name oldPlugins of
            Just _  -> liftIO . warningM "Hsbot.Core.LoadPlugin" $ "Not loading already loaded plugin : " ++ name
            Nothing -> do
                liftIO . infoM "Hsbot.Core.LoadPlugin" $ "Loading plugin : " ++ name
                env <- lift ask
                threadId <- liftIO . forkIO $ runReaderT (runReaderT loop pEnv) env
                let newPlugins = M.insert name (pEnv, threadId) oldPlugins
                put $ bot { botPlugins = newPlugins
                          , botHooks   = chan : botHooks bot }

