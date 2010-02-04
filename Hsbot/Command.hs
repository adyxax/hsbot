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

--
-- TODO Clean this crap
--
dispatchMessage :: BotMsg -> IrcBot ()
dispatchMessage (InputMsg inputMsg) = do
    plugins <- gets botPlugins
    cmds    <- gets botCommands
    case command inputMsg of
        "PRIVMSG" ->  -- The first word matters as the command
                     let msg = (parameters inputMsg) !! 1
                         pfx = commandPrefix config
                     in if (head msg) == pfx
                          then
                            let msg'        = tail msg -- all but the cmd prefix
                                key         = head $ words msg'
                                pluginNames = fromMaybe [] $ M.lookup key cmds
                                plugins'    = fromMaybe [] $ mapM (flip M.lookup plugins) pluginNames
                            in  mapM_ (sendToPlugin $ InternalCmd $ IntCmd ("runCommand " ++ key) inputMsg) plugins'
                          else
                            mapM_ (sendToPlugin (InputMsg inputMsg)) (M.elems plugins)
        _        -> return ()
dispatchMessage _ = return ()

-- | Processes an internal command
processInternalCommand :: BotMsg -> IrcBot ()
processInternalCommand (InternalCmd intCmd) = do
    let command' = words $ internalCommand intCmd
    case command' !! 0 of
        "REGISTER" -> case command' !! 1 of
                        "COMMAND" -> registerCommand (command' !! 2) (command' !! 3)
                        _         -> traceM $ inColor ("Invalid argument for the REGISTER command : " ++ (command' !! 2)) [31]
        _          -> traceM $ inColor ("Invalid command : " ++ (command' !! 1)) [31]
processInternalCommand _ = return ()

