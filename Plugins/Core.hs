module Plugins.Core
    ( mainCore
    ) where

import Control.Concurrent.Chan(Chan)
import Control.Exception
import Control.Monad.State
import Data.Maybe(fromMaybe)
import Prelude hiding (catch)

import Hsbot.IRCPlugin
import Hsbot.Types
import Hsbot.Utils

-- | The plugin's main entry point
mainCore :: Chan BotMsg -> Chan BotMsg -> IO ()
mainCore serverChan chan = do
    let plugin = PluginInstance "Core" serverChan chan
    evalStateT (mapM_ sendRegisterCommand ["list", "load", "reload", "unload"]) plugin
    plugin' <- (execStateT run plugin) `catch` (\(_ :: AsyncException) -> return plugin)
    evalStateT (mapM_ sendUnregisterCommand ["list", "load", "reload", "unload"]) plugin'

-- | The IrcPlugin monad main function
run :: IrcPlugin ()
run = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: BotMsg -> IrcPlugin ()
    eval (InternalCmd intCmd) = do
        let request = intCmdBotMsg intCmd
        case intCmdCmd intCmd of
            "RUN"    -> let stuff = words $ intCmdMsg intCmd
                        in case head stuff of
                            "list"   -> listPlugins request
                            "load"   -> loadPlugin $ tail stuff
                            "reload" -> reloadPlugin $ tail stuff
                            "unload" -> unloadPlugin $ tail stuff
                            _      -> lift $ trace $ show intCmd -- TODO : help message
            "ANSWER" -> let stuff = intCmdMsg intCmd
                        in answerMsg request ("Loaded plugins : " ++ stuff)
            _        -> lift $ trace $ show intCmd
    eval (InputMsg _) = return ()
    eval _ = return ()

-- | The list command
listPlugins :: Maybe IrcMsg -> IrcPlugin ()
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

