module Hsbot.Types
    ( Bot
    , BotState (..)
    , BotStatus (..)
    , BotEnv (..)
    , Env
    , Message (..)
    , PluginState (..)
    ) where

import Control.Concurrent
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import qualified Network.IRC as IRC
import Network.TLS
import System.IO

import Hsbot.Config

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
    , botCommands :: M.Map String [String]
    , botChannels :: [String]
    , botNickname :: String
    }

-- The Plugin monad
data PluginState = PluginState
    { pluginName :: String
    , pluginChan :: Chan Message
    }

data BotStatus = BotContinue | BotExit | BotReload | BotRestart deriving (Show)

data Message = IncomingMsg IRC.Message
             | OutgoingMsg IRC.Message

