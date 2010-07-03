module Hsbot.Irc.Server
    ( initServerConnection
    , sendIrcMsg
    ) where

import Control.Concurrent.Chan
import Control.Monad.State

import Hsbot.Irc.Config
import Hsbot.Irc.Message
import Hsbot.Irc.Types

-- | Setup a newly connected server by sending nick and join stuff
initServerConnection :: IrcConfig -> IrcServer ()
initServerConnection config = do
    sendIrcMsg $ IrcMsg Nothing "NICK" [(ircConfigNickname config)]
    sendIrcMsg $ IrcMsg Nothing "USER" [(ircConfigNickname config), "0", "*", (ircConfigRealname config)]
    when (not . null $ ircConfigPassword config) $ do
        sendIrcMsg $ IrcMsg Nothing "PRIVMSG" ["nickserv", "identify", (ircConfigPassword config)]
    mapM_ joinChan (ircConfigChannels config)

-- | Joins a chan
joinChan :: String -> IrcServer ()
joinChan channel = do
    ircServer <- get
    let oldChannels = ircServerChannels ircServer
    sendIrcMsg $ IrcMsg Nothing "JOIN" [channel]
    put $ ircServer { ircServerChannels = channel : oldChannels }

-- | Sends an IrcMsg
sendIrcMsg :: IrcMsg -> IrcServer ()
sendIrcMsg ircMsg = do
    chan <- gets ircServerChan
    liftIO $ writeChan chan (OutIrcMsg ircMsg)

