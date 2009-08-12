module Hsbot.Main
    ( imain
    , imain'
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import qualified Data.Map as M
import System.IO
import System.Plugins

import qualified Config as C
import Hsbot.Core
import Hsbot.IRC

type Reboot = (Module -> Bot -> IO ())

-- | Bot's first main entry point
imain :: Module -> Reboot -> IO ()
imain modul' reboot = imain' modul' reboot newbot

-- | Bot's main entry point
imain' :: Module -> Reboot -> Bot -> IO ()
imain' modul' reboot bot = do
    -- The chan passing to reboot (or another way to keep it) is still missing
    putStrLn "Connecting servers..."
    let newServers = filter (not . isConnected bot) (ircServers C.config)
    newServers' <- mapM connectServer newServers
    putStrLn "Joining channels..."
    mapM_ initServer newServers'
    putStrLn "Spawning threads..."
    let bot'  = saveServersStates newServers' bot
        Bot x = bot'
    chan <- newChan :: IO (Chan IrcLine)
    mapM_ (forkIO . listener chan) (M.toList x)
    bot'' <- monitor chan bot'
    reboot modul' bot''

-- | Bot main loop, monitors the threads states and handle reboot
monitor :: (Chan IrcLine) -> Bot -> IO Bot
monitor chan bot = do
    loop bot
    where
        loop bot' = do
            input <- readChan chan :: IO IrcLine
            case input of
                 Reboot  -> do
                        putStrLn "Got reboot message, rebooting"
                        return bot'
                 _       -> loop bot'

-- | Thread entry point for socket listeners
listener :: (Chan IrcLine) -> (IrcServer, Handle) -> IO ()
listener chan (server, handle) = forever $ do
    str <- hGetLine handle
    let msg = parseIrcMsg str
    writeChan chan msg
    eval msg
    where
        eval :: IrcLine -> IO ()
        eval (Privmsg (statement, stuff')) = sendPrivmsg (server, handle) stuff'
        eval (Quit (ircServer, handle')) = return ()
        eval (Join (ircServer, handle')) = return ()
        eval (Part (ircServer, handle')) = return ()
        eval (Ping (string)) = do pong handle string
        eval stuff' = case stuff' of
                        Reboot -> return ()
                        Nil    -> return ()

