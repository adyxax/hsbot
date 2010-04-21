module Hsbot.IRC
    ( initServer
    , runServer
    ) where

import Control.Concurrent.Chan
import Control.Monad.State

import Hsbot.Command
import Hsbot.IRCParser
import Hsbot.Types
import Hsbot.Utils

-- | Setup a newly connected server by sending nick and join stuff
initServer :: IrcBot ()
initServer = do
    server <- gets serverConfig
    sendstr $ serializeIrcMsg $ IrcMsg Nothing "NICK" [(nickname server)]
    sendstr $ serializeIrcMsg $ IrcMsg Nothing "USER" [(nickname server), "0", "*", (realname server)]
    when (not . null $ password server) $ do
        sendstr $ serializeIrcMsg $ IrcMsg Nothing "PRIVMSG" ["nickserv", "identify", (password server)]
    mapM_ joinChan (joinChannels server)

-- | Run a server
runServer :: IrcBot ()
runServer = forever $ do
    chan    <- gets botChannel
    let input = readChan chan
    msg <- liftIO input
    case msg of
        InputMsg inputMsg       -> dispatchMessage $ InputMsg inputMsg
        OutputMsg outputMsg     -> sendstr (serializeIrcMsg outputMsg)
        InternalCmd internalCmd -> processInternalCommand $ InternalCmd internalCmd

-- | Joins a chan
joinChan :: String -> IrcBot ()
joinChan name = do
    bot <- get
    let oldChannels = chans bot
        newChannel  = Channel name
                              (nickname $ serverConfig bot)
                              (administrators $ serverConfig bot)
    sendstr $ serializeIrcMsg $ IrcMsg Nothing "JOIN" [name]
    put $ bot { chans = newChannel : oldChannels }

