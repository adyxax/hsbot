module Hsbot.Plugin
    ( spawnIrcPlugins
    ) where

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Monad.State
import qualified Data.Map as M

import Hsbot.Config
import Hsbot.Irc.Config
import Hsbot.Irc.Core
import Hsbot.Types

-- | spawns IrcPlugins
spawnIrcPlugins :: Bot ()
spawnIrcPlugins = do
    config <- gets botConfig
    mapM_ (spawnIrcPlugin) (ircConfigs config)
  where
    spawnIrcPlugin :: IrcConfig -> Bot ()
    spawnIrcPlugin config = do
        bot <- get
        let chan  = botChan bot
        pchan <- liftIO (newChan :: IO (Chan BotMsg))
        threadId <- liftIO $ forkIO (startIrcbot config chan pchan)
        let plugin  = PluginState { pluginName    = ircConfigName config
                                  , pluginChan    = pchan
                                  , pluginHandles = M.empty }
            plugins = botPlugins bot
        put $ bot { botPlugins = M.insert (pluginName plugin) (plugin, threadId) plugins }

