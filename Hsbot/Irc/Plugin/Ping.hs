module Hsbot.Irc.Plugin.Ping
    ( ircBotPluginPing
    ) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.State
import Prelude hiding (catch)

import Hsbot.Irc.Message
import Hsbot.Irc.PluginCommons

-- | The plugin's main entry point
ircBotPluginPing :: Chan IrcBotMsg -> Chan IrcBotMsg -> IO ()
ircBotPluginPing myChan masterChan = do
    threadId <- myThreadId
    let plugin = IrcPluginState { ircPluginName       = "Ping"
                                , ircPluginThreadId   = threadId
                                , ircPluginChan       = myChan
                                , ircPluginMasterChan = masterChan }
    _ <- (execStateT run plugin) `catch` (\(_ :: AsyncException) -> return plugin)
    return ()

-- | The IrcPlugin monad main function
run :: IrcPlugin ()
run = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: IrcBotMsg -> IrcPlugin ()
    eval (InIrcMsg msg)
        | (ircMsgCommand msg) == "PING" = writeMsg . OutIrcMsg $ IrcMsg Nothing "PONG" (ircMsgParameters msg)
        | otherwise = return ()
    eval _ = return ()

