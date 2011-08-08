module Hsbot.Plugin.Admin
    ( admin
    , theAdmin
    ) where

import Control.Concurrent.Chan ()
import Control.Monad.Reader
import qualified Network.IRC as IRC
import Prelude hiding (catch)

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

-- | The Admin plugin identity
admin :: PluginId
admin = PluginId
    { pluginName = "admin"
    , pluginEp   = theAdmin }

-- | An IRC plugin for manage hsbot
theAdmin :: Plugin (Env IO) ()
theAdmin = forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = do
            cmdArgs <- lift $ getCommand msg
            case cmdArgs of
                "exit":"help":_ -> answerMsg msg "exit hsbot."
                "exit":_ -> lift (hasAccess (IRC.msg_prefix msg) Admin) >>= \right -> if right
                        then lift $ setGlobalQuitMVar BotExit
                        else answerMsg msg "Only admins can do that."
                "restart":"help":_ -> answerMsg msg "restart hsbot, reset the running state to config file directives."
                "restart":_ -> lift (hasAccess (IRC.msg_prefix msg) Admin) >>= \right -> if right
                        then lift $ setGlobalQuitMVar BotRestart
                        else answerMsg msg "Only admins can do that."
                "reload":"help":_ -> answerMsg msg "reload hsbot, and try merge the new config file directives with actual running state)."
                "reload":_ -> lift (hasAccess (IRC.msg_prefix msg) Admin) >>= \right -> if right
                        then lift $ setGlobalQuitMVar BotReload
                        else answerMsg msg "Only admins can do that."
                _ -> return ()
        | otherwise = return ()
    eval _ = return ()

