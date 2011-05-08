module Hsbot.Message
    ( answerMsg
    , readMsg
    , writeMsg
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Network.IRC as IRC

import Hsbot.Types

-- Plugin Utils
readMsg :: Plugin (Env IO) Message
readMsg = asks pluginChan >>= liftIO . readChan

writeMsg :: Message -> Plugin (Env IO) ()
writeMsg msg = asks pluginMaster >>= liftIO . flip writeChan msg

answerMsg :: IRC.Message -> String -> Plugin (Env IO) ()
answerMsg _ [] = return ()
answerMsg (IRC.Message (Just (IRC.NickName nick _ _)) _ (channel:_)) msg
    | head channel == '#' = writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [channel, msg]
    | otherwise = writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [nick, msg]
answerMsg _ _ = return ()

