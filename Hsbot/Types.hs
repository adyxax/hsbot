module Hsbot.Types
    ( Bot
    , BotState (..)
    , BotStatus (..)
    , BotEnv (..)
    , Env
    , Message (..)
    , Plugin
    , PluginId (..)
    , PluginState (..)
    ) where

import Control.Concurrent
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Network.TLS
import System.IO

import Hsbot.Config
import Hsbot.Message

-- The bot environment
type Env = ReaderT BotEnv

data BotEnv = BotEnv
    { envHandle      :: Handle
    , envChan        :: Chan Message
    , envQuitMv      :: MVar (BotStatus)
    , envThreadIdsMv :: MVar [ThreadId]
    , envConfig      :: Config
    , envTLS         :: Maybe TLSParams
    , envTLSCtx      :: Maybe TLSCtx
    }

-- The bot monad
type Bot = StateT BotState

data BotState = BotState
    { botPlugins  :: M.Map String (PluginState, MVar (), ThreadId)
    , botHooks    :: [Chan Message]
    , botChannels :: [String]
    , botNickname :: String
    }

-- The Plugin monad
type Plugin = StateT PluginState

data PluginState = PluginState
    { pluginId     :: PluginId
    , pluginChan   :: Chan Message
    , pluginMaster :: Chan Message
    }

data PluginId = PluginId
    { pluginName :: String
    , pluginEp   :: PluginState -> IO ()
    }

data BotStatus = BotContinue | BotExit | BotReload | BotRestart deriving (Show)

