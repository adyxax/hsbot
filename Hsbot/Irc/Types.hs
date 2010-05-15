module Hsbot.Irc.Types
    ( IrcBot
    , IrcBotState (..)
    , IrcServer
    , IrcServerState (..)
    ) where

import Control.Concurrent
import Control.Monad.State
import qualified Data.Map as M
import Data.Time
import System.IO

import Hsbot.Irc.Config
import Hsbot.Irc.Message
import Hsbot.Irc.PluginCommons
import Hsbot.Message

-- | The Ircbot monad
type IrcBot = StateT IrcBotState IO

-- | An Ircbot state
data IrcBotState = IrcBotState
    { ircBotStartTime            :: UTCTime                     -- the bot's uptime
    , ircBotPlugins              :: M.Map String IrcPluginState -- Loaded plugins
    , ircBotCommands             :: M.Map String [String]       -- Loaded plugins
    , ircBotChan                 :: Chan IrcBotMsg              -- The IrcBot's communication channel
    , ircBotMasterChan           :: Chan BotMsg                 -- The Hsbot communication channel
    , ircBotMyChan               :: Chan BotMsg                 -- The Hsbot communication channel
    , ircBotServerState          :: IrcServerState              -- The state of the IrcServer
    , ircBotHandle               :: Handle                      -- The server's socket/handle
    , ircBotConfig               :: IrcConfig                   -- The starting configuration
    , ircBotReaderThreadId       :: ThreadId
    , ircBotMasterReaderThreadId :: ThreadId
    }

-- | The IrcServer monad
type IrcServer = StateT IrcServerState IrcBot

-- | An IRC server
data IrcServerState = IrcServerState
    { ircServerId            :: String         -- the server's address
    , ircServerChannels      :: [String]       -- the Channels we are connected to
    , ircServerNickname      :: String         -- the hsbot's nickname
    , ircServerCommandPrefix :: Char           -- the prefix the ircbot will recognize as commands
    , ircServerChan          :: Chan IrcBotMsg -- the IrcBot channel
    }

