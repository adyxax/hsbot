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
        where loop bot' = do
                input <- readChan chan
                case input of
                    Reboot ->do
                        putStrLn "Got reboot message, rebooting"
                        return bot'
                    Str str -> putStrLn ("received : " ++ str) >> loop bot'

-- | Thread entry point for socket listeners
listener :: (Chan IrcLine) -> (IrcServer, Handle) -> IO ()
listener chan (server, handle) = forever $ do
    str <- hGetLine handle
    writeChan chan (Str str)
    if ping str then pong handle str
      else eval (parseIrcMsg str)
    where
        eval str
            | (Cmd user channel (cmd, args)) <- str = do
                let cmd' = tail cmd
                unless (null cmd') (parseCmds user cmd' args channel)
        parseCmds user cmd args channel
            | cmd == "reboot" = writeChan chan Reboot
            | otherwise       = do
                putStrLn $"Unknown command : " ++ cmd

