module Hsbot.Core
    ( Bot(..)
    , Config(..)
    , IrcServer(..)
    , newbot
    , sendstr
    ) where

import qualified Data.Map as M
import System.IO (Handle)
import Text.Printf (hPrintf)

-- | An IRC Bot server state (socket handles)
data Bot = Bot
    { joinedServers :: M.Map IrcServer Handle -- servers we are connected to
    } deriving (Eq, Show)

-- | Configuration data type
data Config = Config {
   commandPrefixes :: String,   -- command prefixes, for example @[\'>\',\'@\',\'?\']@
   ircServers      :: [IrcServer]  -- list of 'Server's to connect to
} deriving (Eq,Show)

-- | An IRC server
data IrcServer = IrcServer
    { address        :: String   -- the server's address
    , port           :: Int      -- the server's port
    , channels       :: [String] -- a list of channels to join
    , nickname       :: String   -- the hsbot's nickname
    , password       :: String   -- the hsbot's password, optional
    , realname       :: String   -- the hsbot's real name, optional
    , administrators :: [String] -- bot admins nicknames
    } deriving (Eq, Show)

-- | Returns a new, empty bot
newbot :: Bot
newbot = Bot (M.empty)

-- | Send a string over handle
sendstr handle str = hPrintf handle "%s\r\n" str

