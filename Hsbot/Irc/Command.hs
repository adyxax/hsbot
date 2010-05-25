module Hsbot.Irc.Command
    ( processInternalCommand
    , registerCommand
    , unregisterCommand
    ) where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Hsbot.Irc.Message
import Hsbot.Irc.Plugin
import Hsbot.Irc.Types

-- | Registers a plugin's command
registerCommand :: String -> String -> IrcBot ()
registerCommand cmd pluginName' = do
    ircBot  <- get
    let cmds    = ircBotCommands ircBot
        plugins = ircBotPlugins ircBot
    case M.lookup pluginName' plugins of
        Just _  -> let pluginNames = pluginName' : fromMaybe [] (M.lookup cmd cmds) -- TODO : remove/check for duplicates ?
                       newCmds     = M.insert cmd pluginNames cmds
                   in  put $ ircBot { ircBotCommands = newCmds }
        Nothing -> return ()

-- | Unregisters a plugin's command
unregisterCommand :: String -> String -> IrcBot ()
unregisterCommand cmd pluginName' = do
    ircBot    <- get
    let cmds    = ircBotCommands ircBot
        newCmds = M.adjust (L.delete pluginName') cmd cmds
    put $ ircBot { ircBotCommands = newCmds }

-- | Processes an internal command
processInternalCommand :: IrcBotMsg -> IrcBot (Bool)
processInternalCommand (IntIrcCmd ircCmd)
  | ircCmdTo ircCmd == "CORE"   = processCoreCommand ircCmd
  | otherwise = do
      plugins <- gets ircBotPlugins
      case M.lookup (ircCmdTo ircCmd) plugins of
          Just (plugin, _) -> sendToPlugin (IntIrcCmd ircCmd) plugin
          Nothing          -> return ()
      return False
processInternalCommand _ = return (False)

-- | Processes a core command
processCoreCommand :: IrcCmd -> IrcBot (Bool)
processCoreCommand ircCmd = do
    let command' = ircCmdCmd ircCmd
        originalRequest = ircCmdBotMsg ircCmd
    case command' of
        "LIST"       -> listPlugins originalRequest (ircCmdFrom ircCmd)
        "LOAD"       -> loadIrcPlugin $ ircCmdMsg ircCmd
        "REGISTER"   -> registerCommand (ircCmdMsg ircCmd) (ircCmdFrom ircCmd)
        "UNLOAD"     -> unloadIrcPlugin $ ircCmdMsg ircCmd
        "UNREGISTER" -> unregisterCommand (ircCmdMsg ircCmd) (ircCmdFrom ircCmd)
        "UPDATE"     -> processUpdateCommand ircCmd
        _            -> return ()
    return $ command' == "REBOOT"

-- | Process an update command
processUpdateCommand :: IrcCmd -> IrcBot ()
processUpdateCommand ircCmd = do
    ircbot <- get
    let oldData = ircBotResumeData ircbot
        from    = ircCmdFrom ircCmd
        stuff   = ircCmdMsg ircCmd
    put $ ircbot { ircBotResumeData = M.insert from stuff oldData }

