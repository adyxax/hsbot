module Hsbot.Command
    ( dispatchCommand
    , registerCommand
    ) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import Config
import Hsbot.Plugin
import Hsbot.Types
import Hsbot.Utils

-- TODO :  unregister command

dispatchCommand :: BotMsg -> IrcBot ()
dispatchCommand (InputMsg inputMsg) = do
    plugins <- gets botPlugins
    cmds    <- gets botCommands
    if command inputMsg == "PRIVMSG"
      then  -- The first word matters as the command
        let msg = (parameters inputMsg) !! 1
            pfx = commandPrefix config
        in if (head msg) == pfx
             then
               let key         = tail msg -- all but the cmd prefix
                   pluginNames = fromMaybe [] $ M.lookup key cmds
                   plugins'    = fromMaybe [] $ mapM (flip M.lookup plugins) pluginNames
               in mapM_ (sendToPlugin (InputMsg inputMsg)) plugins'
             else
               mapM_ (sendToPlugin (InputMsg inputMsg)) (M.elems plugins)
      else
        return ()
dispatchCommand _ = return ()

registerCommand :: String -> String -> IrcBot ()
registerCommand cmd pluginName' = do
    bot     <- get
    cmds    <- gets botCommands
    exists <- pluginExists pluginName'
    if exists
      then
        let cmds' = if cmd `M.member` cmds
                      then cmds
                      else M.singleton cmd []
            newCmds = M.adjust (++ [pluginName']) cmd cmds'
        in put $ bot { botCommands = newCmds }
      else
        traceM $ inColor ("Couldn't register command \"" ++ cmd ++ "\" for plugin \""
                          ++ pluginName' ++ "\" : plugin does not exists") [31]

