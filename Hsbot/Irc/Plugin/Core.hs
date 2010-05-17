module Hsbot.Irc.Plugin.Core
    ( ircBotPluginCore
    ) where

import Control.Concurrent (Chan)
import Control.Exception
import Control.Monad.State
import Prelude hiding (catch)

import Hsbot.Irc.Message
import Hsbot.Irc.Plugin.Utils

-- | The plugin's main entry point
ircBotPluginCore :: Chan IrcBotMsg -> Chan IrcBotMsg -> IO ()
ircBotPluginCore myChan masterChan = do
    let plugin = IrcPluginState { ircPluginName       = "Core"
                                , ircPluginChan       = myChan
                                , ircPluginMasterChan = masterChan }
    evalStateT (mapM_ sendRegisterCommand ["list", "load", "reload", "unload"]) plugin
    plugin' <- (execStateT run plugin) `catch` (\(_ :: AsyncException) -> return plugin)
    evalStateT (mapM_ sendUnregisterCommand ["list", "load", "reload", "unload"]) plugin'

-- | The IrcPlugin monad main function
run :: IrcPlugin ()
run = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: IrcBotMsg -> IrcPlugin ()
    eval (IntIrcCmd intCmd) = do
        let request = ircCmdBotMsg intCmd
        case ircCmdCmd intCmd of
            "RUN"    -> let stuff = words $ ircCmdMsg intCmd
                        in case head stuff of
                            "list"   -> listPlugins request
                            "load"   -> loadPlugin $ tail stuff
                            "reload" -> reloadPlugin $ tail stuff
                            "unload" -> unloadPlugin $ tail stuff
                            _        -> return () -- TODO : help message
            "ANSWER" -> let stuff = ircCmdMsg intCmd
                        in answerMsg request ("Loaded plugins : " ++ stuff)
            _        -> return ()
    eval _ = return ()

-- | The list command
listPlugins :: IrcMsg -> IrcPlugin ()
listPlugins request = do
    sendCommandWithRequest "LIST" "CORE" (unwords []) request

-- | The load command
loadPlugin :: [String] -> IrcPlugin ()
loadPlugin pluginNames = mapM_ (sendCommand "LOAD" "CORE") pluginNames

-- | The reload command
reloadPlugin :: [String] -> IrcPlugin ()
reloadPlugin pluginNames = mapM_ (sendCommand "RELOAD" "CORE") pluginNames

-- | The unload command
unloadPlugin :: [String] -> IrcPlugin ()
unloadPlugin pluginNames = mapM_ (sendCommand "UNLOAD" "CORE") pluginNames

