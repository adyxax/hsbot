module Hsbot.Core
    ( Bot(..)
    , Channel(..)
    , Config(..)
    , IrcServer(..)
    , IrcBot
    , IrcMsg(..)
    , Plugin(..)
    , connectServer
    , disconnectServer
    , inColor
    , serializeIrcMsg
    , traceM
    , writeMsg
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State
import Data.List
import Network
import System.IO
import System.Plugins
import System.Time (ClockTime, getClockTime)

-- | TODO : a monad for a channel, and a monad for a server, all together driven by a Bot?

-- | Configuration data type
data Config = Config {
   commandPrefixes :: String,   -- command prefixes, for example @[\'>\',\'@\',\'?\']@
   ircServer       :: IrcServer -- list of 'Server's to connect to
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
    { serverConfig   :: IrcServer   -- original server config we are connected to
    , startTime      :: ClockTime   -- the bot's uptime
    , botHandle      :: Handle      -- the socket/handle
    , chans          :: [Channel]   -- the list of channels we have joined
    , botPlugins     :: [Plugin]    -- The list of loaded plugins
    , serverChannel  :: Chan IrcMsg -- The bot's communication channel
    , serverThreadId :: ThreadId    -- The bot's thread ID
    }

instance Show Bot where
    show (Bot _ s h c p _ _) = (show s) ++ (show h) ++ (show c) ++ (show p)

-- | A channel connection
data Channel = Channel
    { channelName   :: String   -- the channel's name
    , channelNick   :: String   -- our nickname
    , channelAdmins :: [String] -- the bot administrators
    } deriving (Show)

-- |An IRC message.
data IrcMsg = IrcMsg
    { prefix     :: Maybe String -- the message prefix
    , command    :: String       -- the message command
    , parameters :: [String]     -- the message parameters
    } deriving (Show)

-- | A plugin definition
data Plugin = Plugin
    { pluginName     :: String      -- The plugin's name
    , pluginModule   :: Module      -- The plugin himself
    , pluginThreadId :: ThreadId    -- The plugin thread
    , pluginChannel  :: Chan IrcMsg -- The plugin channel
    }

instance Show Plugin where
    show (Plugin name _ _ _) = show name

-- Connect to the server and return the initial bot state
connectServer :: IrcServer -> IO Bot
connectServer server = do
    let name  = address server
    starttime <- getClockTime
    putStr $ "Connecting to " ++ name ++ "... "
    handle <- connectTo name $ port server
    hSetBuffering handle NoBuffering
    putStrLn "done."
    putStr $ "Opening server communication channel... "
    chan <- newChan :: IO (Chan IrcMsg)
    threadId <- forkIO $ botWriter handle chan
    putStrLn "done."
    return (Bot server starttime handle [] [] chan threadId)

-- | Disconnect from the server
disconnectServer :: Bot -> IO ()    -- IO Bot ?
disconnectServer bot = do
    killThread $ serverThreadId bot
    hClose $ botHandle bot
    return ()

-- | Processing loop
botWriter :: Handle -> Chan IrcMsg -> IO ()
botWriter handle chan = forever $ do
    input <- readChan chan :: IO IrcMsg
    sendstr handle (serializeIrcMsg input)

-- | Write an IRC message to the bot's writer
writeMsg :: IrcMsg -> IrcBot ()
writeMsg msg = do
    chan <- gets serverChannel
    liftIO $ writeChan chan msg

-- |Serialize an IRC message to a string.
serializeIrcMsg :: IrcMsg -> String
serializeIrcMsg (IrcMsg pfx cmd params) = pfxStr ++ cmd ++ paramStr
    where pfxStr = case pfx of
                        Nothing  -> ""
                        Just pfx' -> ":" ++ pfx' ++ " "
          paramStr = concat (map paramToStr (init params)
                             ++ [lastParamToStr (last params)])
          paramToStr p = " " ++ p
          lastParamToStr p = " :" ++ p

-- | Send a string over handle
sendstr :: Handle -> String -> IO ()
sendstr handle str = do
    trace $ inColor ("--> " ++ str) [33]
    hPutStr handle (str ++ "\r\n")

-- | Log a message string
trace :: String -> IO ()
trace msg = putStrLn msg

-- | Log a message string
traceM :: String -> IrcBot ()
traceM msg = liftIO $ putStrLn msg

-- |Wrap a string with ANSI escape sequences.
inColor :: String -> [Int] -> String
inColor str vals = "\ESC[" ++ valstr ++ "m" ++ str ++ "\ESC[0m"
    where valstr = concat $ intersperse ";" $ map show vals

