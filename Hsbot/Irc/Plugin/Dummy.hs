module Hsbot.Irc.Plugin.Dummy
    ( ircBotPluginDummy
    ) where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.State
import Prelude hiding (catch)

import Hsbot.Irc.Message
import Hsbot.Irc.PluginCommons

-- | The plugin's main entry point
ircBotPluginDummy :: Chan IrcBotMsg -> Chan IrcBotMsg -> IO ()
ircBotPluginDummy myChan masterChan = do
    let plugin = IrcPluginState { ircPluginName       = "Dummy"
                                , ircPluginChan       = myChan
                                , ircPluginMasterChan = masterChan }
    _ <- (execStateT run plugin) `catch` (\(_ :: AsyncException) -> return plugin)
    return ()

-- | The IrcPlugin monad main function
run :: IrcPlugin ()
run = forever $ do
    _ <- readMsg
    return ()

