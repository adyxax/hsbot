module Hsbot.Message
    ( answerMsg
    , readMsg
    , writeMsg
    ) where

import Control.Concurrent
import Control.Monad.State
import qualified Network.IRC as IRC

import Hsbot.Types

-- Plugin Utils
readMsg :: Plugin IO (Message)
readMsg = gets pluginChan >>= liftIO . readChan >>= return

writeMsg :: Message -> Plugin IO ()
writeMsg msg = gets pluginMaster >>= liftIO . flip writeChan msg

answerMsg :: IRC.Message -> String -> Plugin IO ()
answerMsg request msg =
    case IRC.msg_params request of
        sender:_ -> writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [sender, msg]
        [] -> return ()

