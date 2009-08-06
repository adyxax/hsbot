module Hsbot.Main
    ( imain
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
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
    putStrLn "yeah"
    putStrLn "Connecting servers..."
    servers' <- mapM connectServer (ircServers C.config)
    putStrLn "Joining channels..."
    mapM_ initServer servers'
    return ()

-- | Thread entry point for socket listeners
listener :: (Chan IrcOutput) -> (IrcServer, Handle) -> IO ()
listener chan (server, handle) = forever $ do
    str <- hGetLine handle
    writeChan chan (Str str)
    if ping str then pong handle str
      else return ()

