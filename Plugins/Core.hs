module Plugins.Core
    ( mainCore
    ) where

import Control.Concurrent.Chan

import Hsbot.IRCPlugin
import Hsbot.Types
import Hsbot.Utils

-- | The plugin's main entry point
mainCore :: Chan BotMsg -> Chan BotMsg -> IO ()
mainCore serverChan chan = do
    let plugin = PluginInstance "Core" serverChan chan
    (runStateT run plugin) `catch` (const $ return ((), plugin))
    return ()

-- | The IrcPlugin monad main function
run :: IrcPlugin ()
run = do
    mapM_ sendRegisterCommand ["load", "unload"]
    runPlugin
    mapM_ sendUnregisterCommand ["load", "unload"]

runPlugin :: IrcPlugin ()
runPlugin = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: BotMsg -> IrcPlugin ()
    eval (InternalCmd intCmd) = do
        case intCmdCmd intCmd of
            "RUN" -> let stuff = words $ intCmdMsg intCmd
                     in case head stuff of
                            "load"   -> loadPlugin $ tail stuff
                            "unload" -> unloadPlugin $ tail stuff
                            _      -> lift $ trace $ show intCmd -- TODO : help message
            _     -> lift $ trace $ show intCmd
    eval (InputMsg msg) = return ()
    eval _ = return ()

-- | The load command
loadPlugin :: [String] -> IrcPlugin ()
loadPlugin pluginNames = mapM_ (sendCommand "LOAD" "CORE") pluginNames

-- | The unload command
unloadPlugin :: [String] -> IrcPlugin ()
unloadPlugin pluginNames = mapM_ (sendCommand "UNLOAD" "CORE") pluginNames

