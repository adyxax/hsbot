module Hsbot.IRCPlugin
    ( readMsg
    , sendRegisterCommand
    , sendUnregisterCommand
    , writeMsg
    ) where

import Control.Concurrent.Chan
import Control.Monad.State

import Hsbot.Types

-- | Basic input output for IrcPlugins
readMsg :: IrcPlugin (BotMsg)
readMsg = do
    chan  <- gets instanceChan
    input <- liftIO $ readChan chan
    return input

writeMsg :: BotMsg -> IrcPlugin ()
writeMsg botMsg = do
    serverChan <- gets instanceServerChan
    liftIO $ writeChan serverChan $ botMsg

-- | Commands management
sendCommand :: String -> String -> String -> IrcPlugin ()
sendCommand cmd to params = do
    serverChan <- gets instanceServerChan
    from       <- gets instanceName
    liftIO $ writeChan serverChan $ InternalCmd $ IntCmd cmd from to params

sendRegisterCommand :: String -> IrcPlugin ()
sendRegisterCommand cmd = sendCommand "REGISTER" "CORE" cmd

sendUnregisterCommand :: String -> IrcPlugin ()
sendUnregisterCommand cmd = sendCommand "UNREGISTER" "CORE" cmd

-- | a isAdmin helper : I need an admin plugin (to track admins' status around chans)

