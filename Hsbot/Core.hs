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
import Network
import qualified Network.IRC as IRC
import Network.BSD (getHostName)
import Network.TLS
import Prelude hiding (catch)
import System.IO
import System.Log.Logger

import Hsbot.Config
import Hsbot.Types
import Hsbot.Utils

initHsbot :: Config -> IO (BotEnv)
initHsbot config = do
    chan <- newChan :: IO (Chan Message)
    threadIdsMv <- newMVar []
    quitMv <- newEmptyMVar
    let hostname = configAddress config
        port = configPort config
    infoM "Hsbot.Core" $ "Connecting to " ++ hostname -- ++ ":" ++ port
    connhdl <- connectTo hostname port
    hSetBuffering connhdl LineBuffering
    hSetEncoding connhdl utf8
    (tls, tlsCtx) <- case sslOn $ configTLS config of
        True  -> do
            infoM "Hsbot.Core" "TLS init"
            tlsenv <- initTLSEnv (configTLS config)
            randomGen <- makeSRandomGen >>= either (fail . show) (return . id)
            sCtx <- client tlsenv randomGen connhdl
            handshake sCtx
            return (Just tlsenv, Just sCtx)
        False -> return (Nothing, Nothing)
    return BotEnv { envHandle      = connhdl
                  , envChan        = chan
                  , envQuitMv      = quitMv
                  , envThreadIdsMv = threadIdsMv
                  , envConfig      = config
                  , envTLS         = tls
                  , envTLSCtx      = tlsCtx }

runHsbot :: Env IO (BotStatus)
runHsbot = do
    -- First we say hello
    env <- ask
    hostname <- liftIO getHostName
    let connhdl  = envHandle env
        tlsCtx   = envTLSCtx env
        config   = envConfig env
        nickname = head $ configNicknames config
        channels = configChannels config
    liftIO . sendStrToClient connhdl tlsCtx . IRC.encode $ IRC.nick nickname
    liftIO . sendStrToClient connhdl tlsCtx . IRC.encode $ IRC.user nickname hostname "*" (configRealname config)
    mapM_ (liftIO . sendStrToClient connhdl tlsCtx . IRC.encode . IRC.joinChan) channels
    -- Next we spawn the reader thread
    liftIO $ debugM "Hsbot.Core" $ "Spawning reader thread"
    myOwnThreadId  <- liftIO $ myThreadId
    chan <- asks envChan
    (liftIO . forkIO $ botReader connhdl tlsCtx chan myOwnThreadId) >>= addThreadIdToQuitMVar
    -- Then we spawn all plugins
    -- asks envSocket  >>= mapM_ ( ----- what's next? the core server handling! ----- )
    -- Finally we spawn the main bot loop
    --
    -- We wait for the quit signal
    code <- asks envQuitMv >>= liftIO . takeMVar
    -- and we clean things up
    asks envThreadIdsMv >>= liftIO . readMVar >>= liftIO . mapM_ killThread
    return code

botReader :: Handle -> Maybe TLSCtx -> Chan Message -> ThreadId -> IO ()
botReader _ (Just ctx) chan _ = forever $
    recvData ctx >>= return . L.toChunks >>= mapM_ (handleIncomingStr chan . C.unpack)  -- TODO exceptions
botReader handle Nothing chan fatherThreadId = forever $
    (hGetLine handle) `catch` handleIOException >>= handleIncomingStr chan
  where
    handleIOException :: IOException -> IO (String)
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

handleIncomingStr :: Chan Message -> String -> IO ()
handleIncomingStr chan str = do
    case IRC.decode str of
        Just msg -> do
            debugM "Ircd.Reader" $ "<-- " ++ (show msg)
            writeChan chan $ IncomingMsg msg
        Nothing -> debugM "Ircd.Reader" $ "Error: couldn't decode : " ++ str     -- TODO: spam control

terminateHsbot :: Env IO ()
terminateHsbot = do
    liftIO $ infoM "Hsbot.Core" "Closing connection"
    asks envHandle >>= liftIO . hClose

