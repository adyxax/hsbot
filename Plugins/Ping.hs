module Plugins.Ping
    ( mainPing
    ) where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.State
import Prelude hiding (catch)

import Hsbot.IRCPlugin
import Hsbot.Types

-- | The plugin's main entry point
mainPing :: Chan BotMsg -> Chan BotMsg -> IO ()
mainPing serverChan chan = do
    let plugin = PluginInstance "Ping" serverChan chan
    _ <- (execStateT run plugin) `catch` (\(_ :: AsyncException) -> return plugin)
    return ()

-- | The IrcPlugin monad main function
run :: IrcPlugin ()
run = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: BotMsg -> IrcPlugin ()
    eval (InputMsg msg)
        | (command msg) == "PING" = writeMsg $ OutputMsg $ IrcMsg Nothing "PONG" (parameters msg)
        | otherwise = return ()
    eval _ = return ()

