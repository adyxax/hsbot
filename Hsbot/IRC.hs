module Hsbot.IRC
    ( initServer
    , runServer
    ) where

import Control.Concurrent.Chan
import Control.Monad.State
import System.IO

import Hsbot.Core
import Hsbot.IRCParser

-- | Setup a newly connected server by sending nick and join stuff
initServer :: IrcBot ()
initServer = do
    server <- gets serverConfig
    writeMsg $ IrcMsg Nothing "NICK" [(nickname server)]
    writeMsg $ IrcMsg Nothing "USER" [(nickname server), "0", "*", (realname server)]
    when (not . null $ password server) $ do
        writeMsg $ IrcMsg Nothing "PRIVMSG" ["nickserv", "identify", (password server)]
    joinChans
    return ()

-- | Run a server
runServer :: IrcBot ()
runServer = do
    handle <- gets botHandle
    plugins <- gets botPlugins
    str <- liftIO $ hGetLine handle
    traceM $ inColor ("<-- " ++ str) [33]
    let msg = parseIrcMsg str
    case msg of
        Right msg' -> do
            mapM_ (sendPlugin msg') plugins
            return ()
        _ -> do
            return ()
    traceM $ show msg
    runServer

sendPlugin :: IrcMsg -> Plugin -> IrcBot ()
sendPlugin msg plugin = do
    let chan = pluginChannel plugin
    liftIO $ writeChan chan msg

-- | Join chans
joinChans :: IrcBot ()
joinChans = do
    server <- gets serverConfig
    mapM_ joinChan (channels server)

-- | Joins a chan
joinChan :: String -> IrcBot ()
joinChan name = do
    bot <- get
    let oldChannels = chans bot
        newChannel  = Channel name
                              (nickname $ serverConfig bot)
                              (administrators $ serverConfig bot)
    traceM $ "  Joining " ++ name
    writeMsg $ IrcMsg Nothing "JOIN" [name]
    put $ bot { chans = newChannel : oldChannels }

