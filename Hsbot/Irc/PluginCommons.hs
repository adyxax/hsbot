module Hsbot.Irc.PluginCommons
    ( IrcPlugin
    , IrcPluginState (..)
    , answerMsg
    , readMsg
    , sendCommand
    , sendCommandWithRequest
    , sendRegisterCommand
    , sendUnregisterCommand
    , writeMsg
    ) where

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Monad.State
import Data.Maybe (fromMaybe)

import Hsbot.Irc.Message

-- | The IrcPlugin monad
type IrcPlugin = StateT IrcPluginState IO

-- | A plugin state
data IrcPluginState = IrcPluginState
    { ircPluginName       :: String         -- The plugin's name
    , ircPluginChan       :: Chan IrcBotMsg -- The plugin chan
    , ircPluginMasterChan :: Chan IrcBotMsg -- The master's chan
    }

--- | Basic input output for IrcPlugins
readMsg :: IrcPlugin (IrcBotMsg)
readMsg = do
   chan  <- gets ircPluginChan
   input <- liftIO $ readChan chan
   return input

writeMsg :: IrcBotMsg -> IrcPlugin ()
writeMsg (OutIrcMsg msg) = do
   chan <- gets ircPluginMasterChan
   liftIO $ writeChan chan (OutIrcMsg msg)
writeMsg _ = return ()

answerMsg :: IrcMsg -> String -> IrcPlugin ()
answerMsg request msg = do
    let chanOrigin = head $ ircMsgParameters request
        sender     = takeWhile (/= '!') $ fromMaybe "" (ircMsgPrefix request)
    case head chanOrigin of
        '#' -> writeMsg . OutIrcMsg $ IrcMsg Nothing "PRIVMSG" [chanOrigin, msg]
        _   -> writeMsg . OutIrcMsg $ IrcMsg Nothing "PRIVMSG" [sender, msg]

-- | Command management
sendCommand :: String -> String -> String -> IrcPlugin ()
sendCommand cmd to params = sendCommandWithRequest cmd to params emptyIrcMsg

sendCommandWithRequest :: String -> String -> String -> IrcMsg -> IrcPlugin ()
sendCommandWithRequest cmd to params originalRequest = do
    masterChan <- gets ircPluginMasterChan
    from       <- gets ircPluginName
    liftIO . writeChan masterChan . IntIrcCmd $ IrcCmd cmd from to params originalRequest

sendRegisterCommand :: String -> IrcPlugin ()
sendRegisterCommand cmd = sendCommand "REGISTER" "CORE" cmd

sendUnregisterCommand :: String -> IrcPlugin ()
sendUnregisterCommand cmd = sendCommand "UNREGISTER" "CORE" cmd

