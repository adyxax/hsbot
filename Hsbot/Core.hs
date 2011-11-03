module Hsbot.Core
    ( initHsbot
    , runHsbot
    , terminateHsbot
    ) where

import Control.Concurrent
import Control.Exception (IOException, catch)
import Control.Monad.Reader
import Crypto.Random
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.Map as M
import Network
import qualified Network.IRC as IRC
import Network.BSD (getHostName)
import Network.TLS
import Prelude hiding (catch)
import System.IO
import System.Log.Logger
import Text.ParserCombinators.Parsec

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
    hSetBuffering connhdl NoBuffering
    hSetEncoding connhdl utf8
    (tls, tlsCtx) <- if sslOn $ configTLS config
        then (do
            infoM "Hsbot.Core" "Initializing TLS communication"
            tlsenv <- initTLSEnv (configTLS config)
            randomGen <- newGenIO :: IO SystemRandom
            sCtx <- client tlsenv randomGen connhdl
            success <- handshake sCtx
            unless success $ errorM "Hsbot.Core" "TLS handshake failed"  -- TODO: do some usefull error handling
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

runHsbot :: [String] -> Env IO BotStatus
runHsbot die_msgs = do
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
        liftIO . sendStr env connhdl tlsCtx . IRC.encode $ IRC.nick nickname
        liftIO . sendStr env connhdl tlsCtx . IRC.encode $ IRC.user nickname hostname "*" (configRealname config)
        -- Then we join channels
        mapM_ (liftIO . sendStr env connhdl tlsCtx . IRC.encode . IRC.joinChan) channels
        -- We advertise any death message we should
        mapM_ (\msg -> mapM_ (\channel -> liftIO . sendStr env connhdl tlsCtx . IRC.encode $ IRC.Message Nothing "PRIVMSG" [channel, msg]) channels) die_msgs
        -- Finally we set the new bot state
        asks envBotState >>= liftIO . flip putMVar BotState { botPlugins  = M.empty
                                                            , botAccess   = configAccess config
                                                            , botHooks    = []
                                                            , botChannels = channels
                                                            , botNickname = nickname }
    -- | Run the bot itself
    trueRunHsbot :: Env IO BotStatus
    trueRunHsbot = do
        env <- ask
        -- Next we spawn the reader thread
        liftIO $ debugM "Hsbot.Core" "Spawning reader thread"
        let connhdl  = envHandle env
            tlsCtx   = envTLSCtx env
        chan <- asks envChan
        (liftIO . forkIO $ botReader env connhdl tlsCtx chan) >>= addThreadIdToQuitMVar
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

botReader :: BotEnv -> Handle -> Maybe (TLSCtx Handle) -> Chan Message -> IO ()
botReader env handle mctx chan = do
    ioException <- botTrueReader "" `catch` return
    runReaderT (setGlobalQuitMVar $ BotRestart (show ioException, Just "botReader died")) env
  where
    botTrueReader :: String -> IO IOException
    botTrueReader buff = do
        str <- readThis handle mctx
        case parse messages [] (buff ++ str) of
            Right (msgs, trash) -> do
                mapM_ handleMessage msgs
                botTrueReader trash
            Left err -> do
                errorM "Hsbot.Reader" $ "Reader decode error (" ++ show err ++ ") on " ++ str
                botTrueReader ""
    messages = do
        msgs <- option [] $ many1 message
        trash <- option "" $ many1 anyChar
        return (msgs, trash)
    message = do
        mess <- many1 $ noneOf "\r\n"
        end <- string "\r\n" <|> string "\r" <|> string "\n"
        return $ mess ++ end
    handleMessage :: String -> IO ()
    handleMessage str =
        case IRC.decode str of
            Just msg -> do
                debugM "Hsbot.Reader" $ "<-- " ++ show msg
                writeChan chan $ IncomingMsg msg
            Nothing -> return ()
    readThis :: Handle -> Maybe (TLSCtx Handle) -> IO String
    readThis _ (Just ctx) = fmap L.toString (recvData ctx)
    readThis h Nothing = hGetLine h

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
            liftIO $ debugM "Hsbot.Loop" $ "--> " ++ show outMsg
            liftIO . sendStr env connhdl tlsCtx $ IRC.encode outMsg

terminateHsbot :: Env IO ()
terminateHsbot = do
    liftIO $ infoM "Hsbot.Core" "Closing connection"
    asks envHandle >>= liftIO . hClose

