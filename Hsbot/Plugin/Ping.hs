module Hsbot.Plugin.Ping
    ( ping
    , thePing
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.State
import qualified Network.IRC as IRC
import Prelude hiding (catch)

import Hsbot.Message
import Hsbot.Types

ping :: PluginId
ping = PluginId
    { pluginName = "ping"
    , pluginEp   = thePing }

-- | The IrcPlugin monad main function
thePing :: Plugin (Env IO) ()
thePing = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | (IRC.msg_command msg) == "PING" = writeMsg . OutgoingMsg . IRC.Message Nothing "PONG" $ IRC.msg_params msg
        | otherwise = return ()
    eval _ = return ()

