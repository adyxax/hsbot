module Hsbot.IRC
    ( IrcInput(..)
    , IrcOutput(..)
    , connectServer
    , initServer
    , parseIrcMsg
    )where

import Control.Monad
import Network
import qualified Network.IRC as IRC
import System.IO

import Hsbot.Core

type User    = String
type Channel = String
type Command = String
type Args    = [String]

-- | Information from IRC
data IrcInput = Cmd  User Channel (Command, Maybe String) -- a regular command
              | Line User Channel String                  -- a normal line of little significance
              | Err String                                -- an error occured in parsing
    deriving (Eq,Show)

-- | Data that can go over the remote channel
data IrcOutput = Str String                 -- a regular string
               | Quit (IrcServer, Handle)   -- a quit message from a server
               | Join (IrcServer, Channel)  -- joined a channel
               | Part (IrcServer, Channel)  -- parted the channel
               | Reboot                     -- reboot message sent
               | Nil                        -- signifies thread death, only happens after reboot
    deriving (Eq,Show)

-- | Parses an IrcInput
parseIrcMsg :: String -> IrcInput
parseIrcMsg _ = Err "Parsing not yet implemented"

-- | Connects to a server
connectServer :: IrcServer -> IO (IrcServer, Handle)
connectServer server = do
    let name  = address server
        port_number = port server
    handle <- connectTo name (PortNumber $ fromIntegral port_number)
    hSetBuffering handle NoBuffering
    return (server, handle)

-- | Setup a newly connected server by sending nick and join stuff
initServer :: (IrcServer, Handle) -> IO ()
initServer (server, handle) = do
    sendstr handle (IRC.encode . IRC.nick $ nickname server)
    sendstr handle (IRC.encode $ IRC.user (nickname server) "0" "*" (realname server))
    when (not . null $ password server) $ do
        sendstr handle (IRC.encode $ IRC.privmsg "nickserv" ("identify" ++ (password server)))
    mapM_ (sendstr handle . IRC.encode . IRC.joinChan) (channels server)
    return ()

