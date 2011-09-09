module Hsbot.Message
    ( answerMsg
    , getChannel
    , getCommand
    , getDestination
    , getSender
    , readMsg
    , writeMsg
    ) where

import Control.Concurrent
import qualified Data.List as L
import Control.Monad.Reader
import qualified Network.IRC as IRC

import Hsbot.Types

-- Plugin Utils
readMsg :: Plugin (Env IO) Message
readMsg = asks pluginChan >>= liftIO . readChan

-- TODO : remove the Plugin layer since it's useless, and use the envChan instead
writeMsg :: Message -> Plugin (Env IO) ()
writeMsg msg = asks pluginMaster >>= liftIO . flip writeChan msg

answerMsg :: IRC.Message -> String -> Plugin (Env IO) ()
answerMsg _ [] = return ()
answerMsg (IRC.Message (Just (IRC.NickName nick _ _)) _ (channel:_)) msg
    | head channel == '#' = writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [channel, msg]
    | otherwise = writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [nick, msg]
answerMsg _ _ = return ()

-- | Get the channel a message has been posted on
getChannel :: IRC.Message -> String
getChannel (IRC.Message _ _ (channel:_)) = channel
getChannel _ = ""

-- | Get the command in the IRC message if there is one
getCommand :: IRC.Message -> Env IO [String]
getCommand (IRC.Message _ _ (_:msg:[])) = getCommandFrom $ words msg
  where
    getCommandFrom :: [String] -> Env IO [String]
    getCommandFrom (cmd:stuff) = do
        currentBotState <- asks envBotState >>= liftIO . readMVar
        if botNickname currentBotState `L.isPrefixOf` cmd
            then return stuff
            else return []
    getCommandFrom _ = return []
getCommand _ = return []

-- | Get the sender of a message
getSender :: IRC.Message -> String
getSender (IRC.Message (Just (IRC.NickName nick _ _)) _ _) = nick
getSender _ = ""

-- | Get the destination of a message
getDestination :: IRC.Message -> String
getDestination (IRC.Message _ _ (dest:_:[])) = dest
getDestination _ = ""

