module Hsbot.Core
    ( initHsbot
    , runHsbot
    , terminateHsbot
    ) where

import Control.Concurrent
import Control.Exception (IOException, catch)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Network
import qualified Network.IRC as IRC
import Network.BSD (getHostName)
import Network.TLS
import Prelude hiding (catch)
import System.IO
import System.Log.Logger

import Hsbot.Plugin
import Hsbot.Types
import Hsbot.Utils

initHsbot :: Config -> IO BotEnv
initHsbot config = do
    chan <- newChan :: IO (Chan Message)
    botState <- newEmptyMVar
    threadIdsMv <- newMVar []
    quitMv <- newEmptyMVar
    let hostname = configAddress config
        port = configPort config
    infoM "Hsbot.Core" $ "Connecting to " ++ hostname -- ++ ":" ++ port
    connhdl <- connectTo hostname port
    hSetBuffering connhdl LineBuffering
    hSetEncoding connhdl utf8
    (tls, tlsCtx) <- if sslOn $ configTLS config
        then (do
            infoM "Hsbot.Core" "TLS init"
            tlsenv <- initTLSEnv (configTLS config)
            randomGen <- makeSRandomGen >>= either (fail . show) (return . id)
            sCtx <- client tlsenv randomGen connhdl
            handshake sCtx
            return (Just tlsenv, Just sCtx))
        else return (Nothing, Nothing)
    return BotEnv { envBotState    = botState
                  , envHandle      = connhdl
                  , envChan        = chan
                  , envQuitMv      = quitMv
                  , envThreadIdsMv = threadIdsMv
                  , envConfig      = config
                  , envTLS         = tls
                  , envTLSCtx      = tlsCtx }

runHsbot :: Env IO BotStatus
runHsbot = do
    botNotInitialized <- asks envBotState >>= liftIO . isEmptyMVar
    when botNotInitialized runFirstSteps
    trueRunHsbot
  where
    -- | Initialize the dialog with the IRC server
    runFirstSteps :: Env IO ()
    runFirstSteps = do
        env <- ask
        -- First we say hello
        hostname <- liftIO getHostName
        let connhdl  = envHandle env
            tlsCtx   = envTLSCtx env
            config   = envConfig env
            nickname = head $ configNicknames config
            channels = configChannels config
        liftIO . sendStr connhdl tlsCtx . IRC.encode $ IRC.nick nickname
        liftIO . sendStr connhdl tlsCtx . IRC.encode $ IRC.user nickname hostname "*" (configRealname config)
        -- Then we join channels
        mapM_ (liftIO . sendStr connhdl tlsCtx . IRC.encode . IRC.joinChan) channels
        -- Finally we set the new bot state
        asks envBotState >>= liftIO . (flip putMVar BotState { botPlugins  = M.empty
                                                             , botHooks    = []
                                                             , botChannels = channels
                                                             , botNickname = nickname })
    -- | Run the bot itself
    trueRunHsbot :: Env IO BotStatus
    trueRunHsbot = do
        env <- ask
        -- Next we spawn the reader thread
        liftIO $ debugM "Hsbot.Core" "Spawning reader thread"
        let connhdl  = envHandle env
            tlsCtx   = envTLSCtx env
        myOwnThreadId  <- liftIO myThreadId
        chan <- asks envChan
        (liftIO . forkIO $ botReader connhdl tlsCtx chan myOwnThreadId) >>= addThreadIdToQuitMVar
        -- Then we spawn all plugins
        asks envConfig >>= mapM_ loadPlugin . configPlugins
        -- Finally we spawn the main bot loop
        (liftIO . forkIO $ runReaderT botLoop env) >>= addThreadIdToQuitMVar
        -- We wait for the quit signal
        code <- asks envQuitMv >>= liftIO . takeMVar
        -- and we clean things up
        asks envThreadIdsMv >>= liftIO . readMVar >>= liftIO . mapM_ killThread
        -- TODO : kill plugin threads
        return code

botReader :: Handle -> Maybe TLSCtx -> Chan Message -> ThreadId -> IO ()
botReader _ (Just ctx) chan _ = forever $
    fmap L.toChunks (recvData ctx) >>= mapM_ (handleIncomingStr chan . C.unpack)  -- TODO exceptions
botReader handle Nothing chan fatherThreadId = forever $
    hGetLine handle `catch` handleIOException >>= handleIncomingStr chan
  where
    handleIOException :: IOException -> IO String
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

handleIncomingStr :: Chan Message -> String -> IO ()
handleIncomingStr chan str =
    case IRC.decode str of
        Just msg -> do
            debugM "Ircd.Reader" $ "<-- " ++ show msg
            writeChan chan $ IncomingMsg msg
        Nothing -> debugM "Ircd.Reader" $ "Error: couldn't decode : " ++ str     -- TODO: spam control

botLoop :: Env IO ()
botLoop = forever $ do
    chan <- asks envChan
    msg  <- liftIO $ readChan chan
    hooks <- asks envBotState >>= liftIO . flip withMVar (return . botHooks)
    mapM_ (liftIO . flip writeChan msg) hooks
    case msg of
        IncomingMsg _ -> return () -- TODO parse for core commands
        OutgoingMsg outMsg -> do
            env <- ask
            let connhdl  = envHandle env
                tlsCtx   = envTLSCtx env
            liftIO $ debugM "Ircd.Reader" $ "--> " ++ show outMsg
            liftIO . sendStr connhdl tlsCtx $ IRC.encode outMsg

terminateHsbot :: Env IO ()
terminateHsbot = do
    liftIO $ infoM "Hsbot.Core" "Closing connection"
    asks envHandle >>= liftIO . hClose

