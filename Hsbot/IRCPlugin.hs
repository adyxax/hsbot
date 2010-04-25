module Hsbot.IRCPlugin
    ( IrcPlugin
    , PluginState(..)
    , answerMsg
    , readMsg
    , sendCommand
    , sendCommandWithRequest
    , sendRegisterCommand
    , sendUnregisterCommand
    , writeMsg
    ) where

import Control.Concurrent.Chan
import Control.Monad.State
import Data.Maybe(fromMaybe)

import Hsbot.Types

-- | The IrcPlugin monad
type IrcPlugin a = StateT PluginState IO a

-- | An IRCPlugin state
data PluginState = PluginState
    { instanceName       :: String      -- The plugin's name
    , instanceServerChan :: Chan BotMsg -- The server channel
    , instanceChan       :: Chan BotMsg -- The plugin channel
    }

-- | Basic input output for IrcPlugins
readMsg :: IrcPlugin (BotMsg)
readMsg = do
    chan  <- gets instanceChan
    input <- liftIO $ readChan chan
    return input

writeMsg :: BotMsg -> IrcPlugin ()
writeMsg botMsg = do
    serverChan <- gets instanceServerChan
    liftIO . writeChan serverChan $ botMsg

answerMsg :: IrcMsg -> String -> IrcPlugin ()
answerMsg request msg = do
    let chanOrigin = head $ parameters request
        sender     = takeWhile (/= '!') $ fromMaybe "" (prefix request)
    case head chanOrigin of
        '#' -> writeMsg . OutputMsg $ IrcMsg Nothing "PRIVMSG" [chanOrigin, msg]
        _   -> writeMsg . OutputMsg $ IrcMsg Nothing "PRIVMSG" [sender, msg]

-- | Commands management
sendCommand :: String -> String -> String -> IrcPlugin ()
sendCommand cmd to params = sendCommandWithRequest cmd to params emptyIrcMsg

sendCommandWithRequest :: String -> String -> String -> IrcMsg -> IrcPlugin ()
sendCommandWithRequest cmd to params originalRequest = do
    serverChan <- gets instanceServerChan
    from       <- gets instanceName
    liftIO . writeChan serverChan . InternalCmd $ IntCmd cmd from to params originalRequest

sendRegisterCommand :: String -> IrcPlugin ()
sendRegisterCommand cmd = sendCommand "REGISTER" "CORE" cmd

sendUnregisterCommand :: String -> IrcPlugin ()
sendUnregisterCommand cmd = sendCommand "UNREGISTER" "CORE" cmd

-- | a isAdmin helper : I need an admin plugin (to track admins' status around chans)

