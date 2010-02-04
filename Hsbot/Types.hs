module Hsbot.Types
    ( Bot(..)
    , BotMsg(..)
    , Channel(..)
    , Config(..)
    , IntCmd(..)
    , IrcServer(..)
    , IrcBot
    , IrcMsg(..)
    , Plugin(..)
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State
import qualified Data.Map as M
import Network
import System.IO
import System.Plugins
import System.Time (ClockTime)

-- | TODO : a monad for a channel, and a monad for a server, all together driven by a Bot?

-- | Configuration data type
data Config = Config
    { commandPrefix :: Char      -- command prefixes, for example @[\'>\',\'@\',\'?\']@
    , ircServer     :: IrcServer -- list of 'Server's to connect to
    } deriving (Show)

-- | An IRC server
data IrcServer = IrcServer
    { address        :: String   -- the server's address
    , port           :: PortID   -- the server's port
    , channels       :: [String] -- a list of channels to join
    , nickname       :: String   -- the hsbot's nickname
    , password       :: String   -- the hsbot's password, optional
    , realname       :: String   -- the hsbot's real name, optional
    , administrators :: [String] -- bot admins nicknames
    }

instance Show IrcServer where
    show (IrcServer a p c n pa r ad) = (show a)
                                        ++ (case p of
                                            PortNumber num -> show num
                                            Service s      -> show s
                                            UnixSocket u   -> show u)
                                        ++ (show c) ++ (show n) ++ (show pa) ++ (show r) ++ (show ad)

-- instance Show PortID where
--     show (PortNumber n) = show n
--     show (Service s)    = show s
--     show (UnixSocket g) = show g

-- | The IrcBot monad
type IrcBot a = StateT Bot IO a

-- | An IRC Bot server state
data Bot = Bot
    { serverConfig   :: IrcServer             -- original server config we are connected to
    , startTime      :: ClockTime             -- the bot's uptime
    , botHandle      :: Handle                -- the socket/handle
    , chans          :: [Channel]             -- the list of channels we have joined
    , botPlugins     :: M.Map String Plugin   -- Loaded plugins
    , botChannel     :: Chan BotMsg           -- The bot's communication channel
    , readerThreadId :: ThreadId              -- The bot's thread ID
    , botCommands    :: M.Map String [String] -- Registered commands ("command", ["pluginName"])
    }

instance Show Bot where
    show (Bot _ s h c p _ _ cmds) = (show s) ++ (show h) ++ (show c) ++ (show p) ++ (show cmds)

-- | A channel connection
data Channel = Channel
    { channelName   :: String   -- the channel's name
    , channelNick   :: String   -- our nickname
    , channelAdmins :: [String] -- the bot administrators
    } deriving (Show)

-- | An IRC message
data IrcMsg = IrcMsg
    { prefix     :: Maybe String -- the message prefix
    , command    :: String       -- the message command
    , parameters :: [String]     -- the message parameters
    } deriving (Show)

-- | An internal command
data IntCmd = IntCmd
    { intCmd :: String -- the internal command
    , intMsg :: IrcMsg -- the IrcMsg associated with the command
    }

-- | A plugin definition
data Plugin = Plugin
    { pluginName     :: String      -- The plugin's name
    , pluginModule   :: Module      -- The plugin himself
    , pluginThreadId :: ThreadId    -- The plugin thread
    , pluginChannel  :: Chan BotMsg -- The plugin channel
    }

instance Show Plugin where
    show (Plugin name _ _ _) = show name

data BotMsg = InputMsg IrcMsg | OutputMsg IrcMsg | InternalCmd IntCmd

