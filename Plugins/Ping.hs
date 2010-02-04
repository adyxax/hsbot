module Plugins.Ping
    ( mainPing
    ) where

import Control.Concurrent.Chan

import Hsbot.Types

mainPing :: Chan BotMsg -> Chan BotMsg -> IO ()
mainPing serverChan chan = do
    loop
    where
        loop = do
            input <- readChan chan
            eval input
            loop
        eval :: BotMsg -> IO ()
        eval (InputMsg msg)
            | (command msg) == "PING" = writeChan serverChan $ OutputMsg $ IrcMsg Nothing "PONG" (parameters msg)
            | otherwise = return ()
        eval _ = return ()

