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
answerMsg request msg =
    case IRC.msg_params request of
        sender:_ -> writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [sender, msg]
        [] -> return ()

