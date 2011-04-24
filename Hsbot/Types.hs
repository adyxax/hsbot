module Hsbot.Types
    ( Bot
    , BotState (..)
    , BotStatus (..)
    , BotEnv (..)
    , Config (..)
    , Env
    , Message (..)
    , Plugin
    , PluginId (..)
    , PluginState (..)
    , TLSConfig (..)
    ) where

import Control.Concurrent
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Network
import qualified Network.IRC as IRC
import Network.TLS
import System.IO

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

-- Messaging
data Message = IncomingMsg IRC.Message
             | OutgoingMsg IRC.Message

data BotStatus = BotContinue | BotExit | BotReload | BotRestart deriving (Show)

-- Config
data Config = Config
    { configErrors    :: Maybe String
    , configTLS       :: TLSConfig
    , configAddress   :: String
    , configPort      :: PortID
    , configChannels  :: [String]
    , configNicknames :: [String]
    , configRealname  :: String
    , configPlugins   :: [(String, Chan Message -> Chan Message -> IO ())]
    }

data TLSConfig = TLSConfig
    { sslOn       :: Bool
    , sslCert     :: String
    , sslKey      :: String
    , sslVersions :: [Network.TLS.Version]
    , sslCiphers  :: [Network.TLS.Cipher]
    , sslVerify   :: Bool
    } deriving (Show)

