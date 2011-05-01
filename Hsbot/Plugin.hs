module Hsbot.Plugin
    ( loadPlugin
    ) where

import Control.Concurrent
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import System.Log.Logger

import Hsbot.Types

loadPlugin :: PluginId -> Bot (Env IO) ()
loadPlugin pId = do
    bot <- get
    chan <- liftIO (newChan :: IO (Chan Message))
    master <- lift $ asks envChan
    let name = pluginName pId
        loop = pluginEp pId
        oldPlugins = botPlugins bot
        pState = PluginState { pluginId     = pId
                             , pluginChan   = chan
                             , pluginMaster = master }
    -- We check for unicity
    case M.lookup name oldPlugins of
        Just _  -> liftIO . warningM "Hsbot.Core.LoadPlugin" $ "Not loading already loaded plugin : " ++ name
        Nothing -> do
            liftIO . infoM "Hsbot.Core.LoadPlugin" $ "Loading plugin : " ++ name
            env <- lift ask
            finalStateMVar <- liftIO newEmptyMVar
            threadId <- liftIO . forkIO $ runReaderT (execStateT loop pState >>= storeFinalState finalStateMVar) env
            let newPlugins = M.insert name (pState, finalStateMVar, threadId) oldPlugins
            put $ bot { botPlugins = newPlugins
                      , botHooks   = chan : botHooks bot }
  where
    storeFinalState :: MVar PluginState -> PluginState -> Env IO ()
    storeFinalState finalStateMVar finalState = liftIO $ putMVar finalStateMVar finalState

