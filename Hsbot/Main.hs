module Hsbot.Main
    ( imain
    ) where

import Control.Exception
import Control.Monad.State
import Prelude hiding (catch)
import System.IO()

import Config
import Hsbot.Core
import Hsbot.IRC
import Hsbot.Plugin
import Hsbot.Types

-- | Bot's main entry point
imain :: IO ()
imain = do
    bot <- connectServer $ ircServer config
    bot' <- (execStateT run bot) `catch` (\(_ :: IOException) -> return bot)
    evalStateT disconnectServer bot'

-- | The Bot monad main function
run :: IrcBot ()
run = do
    initServer
    mapM_ loadPlugin defaultPlugins
    runServer

