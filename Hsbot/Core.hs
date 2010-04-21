module Hsbot.Core
    ( connectServer
    , disconnectServer
    ) where

import Control.Concurrent
import Control.Concurrent.Chan()
import Control.Monad.State
import Data.List()
import qualified Data.Map as M
import Network
import System.IO
import System.Time (getClockTime)

import Hsbot.IRCParser
import Hsbot.Plugin
import Hsbot.Types
import Hsbot.Utils

-- Connect to the server and return the initial bot state
connectServer :: IrcServer -> IO Bot
connectServer server = do
    let name  = serverAddress server
    starttime <- getClockTime
    putStr $ "Connecting to " ++ name ++ "... "
    handle <- connectTo name $ serverPort server
    hSetBuffering handle NoBuffering
    putStrLn "done."
    putStr $ "Opening server communication channel... "
    chan <- newChan :: IO (Chan BotMsg)
    threadId <- forkIO $ botReader handle chan
    putStrLn "done."
    return $ Bot server starttime handle [] M.empty chan threadId M.empty

-- | Disconnect from the server
disconnectServer :: IrcBot ()
disconnectServer = do
    bot <- get
    mapM_ unloadPlugin (M.keys $ botPlugins bot)
    liftIO $ killThread $ readerThreadId bot
    liftIO $ hClose $ botHandle bot
    return ()

-- | Socket reading loop
botReader :: Handle -> Chan BotMsg -> IO ()
botReader handle chan = forever $ do
    -- TODO : detect end of connection!
    str <- hGetLine handle
    let msg = parseIrcMsg str
    case msg of
        Right msg' -> do
            trace $ inColor ("<-- " ++ (show msg')) [33]
            writeChan chan (InputMsg msg')
        _ -> do
            return ()

