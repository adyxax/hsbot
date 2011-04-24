module Hsbot.Message
    ( readMsg
    , writeMsg
    ) where

import Control.Concurrent
import Control.Monad.State

import Hsbot.Types

-- Plugin Utils
readMsg :: Plugin IO (Message)
readMsg = do
    chan <- gets pluginChan
    liftIO $ readChan chan >>= return

writeMsg :: Message -> Plugin IO ()
writeMsg msg = do
   chan <- gets pluginMaster
   liftIO $ writeChan chan msg

