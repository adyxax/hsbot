module Hsbot.IRC
    ( initServer
    , runServer
    ) where

import Control.Concurrent.Chan
import Control.Monad.State
import qualified Data.Map as M

import Hsbot.IRCParser
import Hsbot.Plugin
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
    mapM_ joinChan (channels server)

-- | Run a server
runServer :: IrcBot ()
runServer = do
    chan    <- gets botChannel
    plugins <- gets botPlugins
    let input = readChan chan
    msg <- liftIO input
    case msg of
        InputMsg inputMsg ->
            mapM_ (sendToPlugin (InputMsg inputMsg) . snd) (M.toList plugins)
        OutputMsg outputMsg ->
            sendstr (serializeIrcMsg outputMsg)
        InternalCmd internalCmd ->
            traceM "TODO"
    runServer

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

