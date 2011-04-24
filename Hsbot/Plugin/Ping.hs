module Hsbot.Plugin.Ping
    ( pingId
    , ping
    ) where

import Control.Concurrent.Chan ()
import Control.Exception
import Control.Monad.State (execStateT, forever)
import qualified Network.IRC as IRC
import Prelude hiding (catch)

import Hsbot.Message
import Hsbot.Types

pingId :: PluginId
pingId = PluginId
    { pluginName = "ping"
    , pluginEp   = ping }

-- | The plugin's main entry point
ping :: PluginState -> IO ()
ping state = do
    _ <- (execStateT run state) `catch` (\(_ :: AsyncException) -> return state)
    return ()

-- | The IrcPlugin monad main function
run :: Plugin IO ()
run = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: Message -> Plugin IO ()
    eval (IncomingMsg msg)
        | (IRC.msg_command msg) == "PING" = writeMsg . OutgoingMsg . IRC.Message Nothing "PONG" $ IRC.msg_params msg
        | otherwise = return ()
    eval _ = return ()

