module Hsbot.Types
    ( Bot
    , BotMsg (..)
    , BotState (..)
    , Msg (..)
    , Plugin
    , PluginState (..)
    ) where

import Control.Concurrent
import Control.Monad.State
import qualified Data.Map as M
import Data.Time
import System.IO

import Hsbot.Config

-- | The Bot monad
type Bot = StateT BotState IO

-- | An Hsbot state
data BotState = BotState
    { botStartTime  :: UTCTime      -- the bot's uptime
    , botPlugins    :: M.Map String (PluginState, ThreadId) -- Loaded plugins
    , botChan       :: Chan BotMsg  -- the bot's communication channel
    , botConfig     :: Config       -- the bot's starting config
    , botMVar       :: MVar String  -- the place where to put resume data
    , botResumeData :: M.Map String String  -- the necessary data to resume the bot's operations on reboot
    }

-- | The Plugin monad
type Plugin = StateT PluginState IO

-- | A plugin state
data PluginState = PluginState
    { pluginName       :: String              -- The plugin's name
    , pluginChan       :: Chan BotMsg         -- The plugin chan
    , pluginHandles    :: M.Map String Handle -- the plugins's handles
    }

-- | A hsbot message
data Msg = Msg
    { msgType  :: String -- the message type
    , msgFrom  :: String -- who issues it
    , msgTo    :: String -- who it is destinated to
    , msgStuff :: String -- the message to be transfered
    } deriving (Show)

data BotMsg = InMsg Msg | OutMsg Msg | IntMsg Msg deriving (Show)

