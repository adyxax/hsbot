module Hsbot.Main
    ( imain
    ) where

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
    putStrLn "Connecting servers..."
    servers' <- mapM connectServer (ircServers C.config)
    putStrLn "Joining channels..."
    mapM_ initServer servers'
    return ()

