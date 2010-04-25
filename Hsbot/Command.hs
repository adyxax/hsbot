module Hsbot.Command
    ( dispatchMessage
    , processInternalCommand
    , registerCommand
    , unregisterCommand
    ) where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Config
import Hsbot.Plugin
import Hsbot.Types
import Hsbot.Utils

-- | Registers a plugin's command
registerCommand :: String -> String -> IrcBot ()
registerCommand cmd pluginName' = do
    bot     <- get
    cmds    <- gets botCommands
    plugins <- gets botPlugins
    case M.lookup pluginName' plugins of
        Just _  -> let pluginNames = pluginName' : fromMaybe [] (M.lookup cmd cmds) -- TODO : remove/check for duplicates ?
                       newCmds     = M.insert cmd pluginNames cmds
                   in  put $ bot { botCommands = newCmds }
        Nothing -> traceM $ inColor ("Couldn't register command \"" ++ cmd ++ "\" for plugin \""
                                     ++ pluginName' ++ "\" : plugin does not exists") [31]

-- | Unregisters a plugin's command
unregisterCommand :: String -> String -> IrcBot ()
unregisterCommand cmd pluginName' = do
    bot    <- get
    cmds   <- gets botCommands
    let newCmds = M.adjust (L.delete pluginName') cmd cmds
    put $ bot { botCommands = newCmds }

-- | Dispatches an input message
dispatchMessage :: BotMsg -> IrcBot ()
dispatchMessage (InputMsg inputMsg)
    | isPluginCommand = do
        plugins <- gets botPlugins
        cmds    <- gets botCommands
        let key         = tail . head $ words getMsgContent
            pluginNames = fromMaybe [] $ M.lookup key cmds
            plugins'    = fromMaybe [] $ mapM (flip M.lookup plugins) pluginNames
        mapM_ (sendRunCommand $ tail getMsgContent) plugins'
    | otherwise       = do
        plugins <- gets botPlugins
        mapM_ (sendToPlugin (InputMsg inputMsg)) (M.elems plugins)
  where
    isPluginCommand :: Bool
    isPluginCommand =
        and [ command inputMsg == "PRIVMSG"
            , (head getMsgContent) == (commandPrefix config) ]
    sendRunCommand :: String -> Plugin -> IrcBot ()
    sendRunCommand cmd plugin = do
        sendToPlugin (InternalCmd $ IntCmd "RUN" "CORE" (pluginName plugin) cmd inputMsg) plugin
    getMsgContent :: String
    getMsgContent = unwords . tail $ parameters inputMsg
dispatchMessage _ = return ()

-- | Processes an internal command
processInternalCommand :: BotMsg -> IrcBot ()
processInternalCommand (InternalCmd intCmd) = do
    plugins <- gets botPlugins
    if intCmdTo intCmd == "CORE"
      then processCoreCommand intCmd
      else case M.lookup (intCmdTo intCmd) plugins of
          Just plugin -> sendToPlugin (InternalCmd intCmd) plugin
          Nothing     -> errorM $ "Invalid destination in message : " ++ (show intCmd)
processInternalCommand _ = return ()

-- | Processes a core command
processCoreCommand :: IntCmd -> IrcBot ()
processCoreCommand intCmd = do
    let command' = intCmdCmd intCmd
        originalRequest = intCmdBotMsg intCmd
    case command' of
        "LIST"       -> listPlugins originalRequest (intCmdFrom intCmd)
        "LOAD"       -> traceM $ inColor "hsbot has been compiled in static mode." [31]
        "UNLOAD"     -> unloadPlugin $ intCmdMsg intCmd
        "REGISTER"   -> registerCommand (intCmdMsg intCmd) (intCmdFrom intCmd)
        "UNREGISTER" -> unregisterCommand (intCmdMsg intCmd) (intCmdFrom intCmd)
        _            -> traceM $ inColor ("Invalid command : " ++ (show intCmd)) [31]

