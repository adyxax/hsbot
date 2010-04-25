module Hsbot.Core
    ( connectServer
    , disconnectServer
    ) where

import Control.Concurrent
import Control.Concurrent.Chan()
import Control.Exception(IOException, catch)
import Control.Monad.State
import Data.List()
import qualified Data.Map as M
import Network
import Prelude hiding (catch)
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
    putStr "Opening server communication channel... "
    chan <- newChan :: IO (Chan BotMsg)
    myFatherThreadId <- myThreadId
    threadId <- forkIO $ botReader handle chan myFatherThreadId
    putStrLn "done."
    return $ Bot server starttime handle [] M.empty chan threadId M.empty

-- | Disconnect from the server
disconnectServer :: IrcBot ()
disconnectServer = do
    bot <- get
    let name = serverAddress $ serverConfig bot
    liftIO $ putStr "Shutting down plugins..."
    mapM_ unloadPlugin (M.keys $ botPlugins bot)
    liftIO $ putStrLn"done."
    liftIO $ putStr "Closing server communication channel... "
    liftIO . killThread $ readerThreadId bot
    liftIO $ putStrLn "done."
    liftIO . putStr $ "Disconnecting from " ++ name ++ "... "
    liftIO . hClose $ botHandle bot
    liftIO $ putStrLn "done."

-- | Socket reading loop
botReader :: Handle -> Chan BotMsg -> ThreadId -> IO ()
botReader handle chan fatherThreadId = forever $ do
    str <- (hGetLine handle) `catch` handleIOException
    let msg = parseIrcMsg str
    case msg of
        Right msg' -> do
            trace $ inColor ("<-- " ++ (show msg')) [33]
            writeChan chan (InputMsg msg')
        _ -> do
            return ()
  where
    handleIOException :: IOException -> IO (String)
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

