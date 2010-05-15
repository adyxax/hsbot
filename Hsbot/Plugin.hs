module Hsbot.Plugin
    ( Plugin
    , PluginState (..)
    ) where

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Monad.State
import qualified Data.Map as M
import IO (Handle)

import Hsbot.Message

-- | The Plugin monad
type Plugin = StateT PluginState IO

-- | A plugin state
data PluginState = PluginState
    { pluginName       :: String              -- The plugin's name
    , pluginThreadId   :: ThreadId            -- The plugin thread
    , pluginChan       :: Chan BotMsg         -- The plugin chan
    , pluginHandles    :: M.Map String Handle -- the plugins's handles
    }

