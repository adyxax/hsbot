module Hsbot.PluginUtils
    ( sendToPlugin
    ) where

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Monad.State

import Hsbot.Types

-- | Sends a msg to a plugin
sendToPlugin :: BotMsg -> PluginState -> Bot ()
sendToPlugin botMsg plugin = do
    liftIO $ writeChan (pluginChan plugin) botMsg

