module Hsbot.IRC
    ( IrcLine(..)
    , connectServer
    , initServer
    , parseIrcMsg
    , ping
    , pong
    , sendPrivmsg
    )where

import Control.Monad
import Data.List(isPrefixOf)
import Data.Maybe
import Network
import qualified Network.IRC as IRC
import System.IO

import Hsbot.Core
import Hsbot.IRCParser

type User    = String
type Channel = String
type Command = String
type Args    = [String]

-- | An IRC line
data IrcLine = Privmsg (String, [String])   -- statement (chan, sentence...)
               | Quit (IrcServer, Handle)   -- a quit message from a server
               | Join (IrcServer, Channel)  -- joined a channel
               | Part (IrcServer, Channel)  -- parted the channel
               | Ping (String)              -- pinged by the server
               | Reboot                     -- reboot message sent
               | Nil                        -- signifies thread death, only happens after reboot
    deriving (Eq,Show)

-- | Parses an IrcInput
parseIrcMsg :: String -> IrcLine
parseIrcMsg str = 
    case (ircParser str) of
        Left err -> Nil
        Right x  -> eval x
    where
        eval :: IrcMsg -> IrcLine
        eval x@(IrcMsg statement cmd stuff)
            | cmd == "PING" = Ping $ head stuff
            | cmd == "PRIVMSG" =
		    case statement of
		        Nothing -> Nil
			Just statement' -> if stuff!!1 == "reboot" then Reboot
                                              else  Privmsg $ (statement', stuff)
            | otherwise = Nil

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

-- | Check if a message is a PING
ping :: String -> Bool
ping = isPrefixOf "PING :"

-- | Send a pong message given a ping message
pong :: Handle -> String -> IO ()
pong handle str = sendstr handle $ "PONG " ++ (drop 5 str)

sendPrivmsg :: (IrcServer, Handle) -> [String] -> IO ()
sendPrivmsg (server, handle) stuff' = sendstr handle (IRC.encode $ IRC.privmsg (head stuff') (unwords . tail $ stuff'))

