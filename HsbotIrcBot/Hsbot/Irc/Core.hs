module Hsbot.Irc.Core
    ( startIrcbot
    ) where

import Control.Concurrent
import Control.Exception (AsyncException, Handler (..), IOException, catch, catches)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Network
import Prelude hiding (catch)
import System.IO

import Hsbot.Irc.CLI
import Hsbot.Irc.Command
import Hsbot.Irc.Config
import Hsbot.Irc.Message
import Hsbot.Irc.Plugin
import Hsbot.Irc.Server
import Hsbot.Irc.Types

-- | IrcBot's main entry point
startIrcbot :: Options -> IrcConfig -> IO ()
startIrcbot opts ircConfig = do
    when (optDebug opts) $ putStrLn "[IrcBot] Opening communication channel... "
    chan <- newChan :: IO (Chan IrcBotMsg)
    when (optDebug opts) . putStrLn $ concat ["[IrcBot] Connecting to ", ircConfigAddress ircConfig, "... "]
    handle <- connectTo (ircConfigAddress ircConfig) (ircConfigPort ircConfig)
    hSetBuffering handle NoBuffering
    hSetEncoding handle utf8
    when (optDebug opts) $ putStrLn "[IrcBot] Spawning reader thread..."
    myOwnThreadId  <- myThreadId
    readerThreadId <- forkIO $ ircBotReader handle chan myOwnThreadId
    when (optDebug opts) $ putStrLn "[IrcBot] Initializing server connection..."
    let ircServerState = IrcServerState { ircServerId            = ircConfigAddress ircConfig
                                        , ircServerChannels      = []
                                        , ircServerNickname      = ircConfigNickname ircConfig
                                        , ircServerCommandPrefix = ircConfigCommandPrefix ircConfig
                                        , ircServerChan          = chan }
        ircBotState = IrcBotState { ircBotPlugins              = M.empty
                                  , ircBotCommands             = M.empty
                                  , ircBotChan                 = chan
                                  , ircBotServerState          = ircServerState
                                  , ircBotHandle               = handle
                                  , ircBotConfig               = ircConfig }
    ircBotState' <- execStateT (initBotServerConnection ircConfig) ircBotState
    when (optDebug opts) $ putStrLn "[IrcBot] Spawning plugins..."
    ircBotState'' <- execStateT spawnIrcPlugins ircBotState'
    when (optDebug opts) $ putStrLn "[IrcBot] Entering Core loop... "
    (_, ircBotState''') <- runLoop ircBotState''
    when (optDebug opts) $ putStrLn "[IrcBot] Killing reader thread..."
    killThread readerThreadId
    when (optDebug opts) $ putStrLn "[IrcBot] Killing active plugins... "
    evalStateT (mapM_ killIrcPlugin . M.keys $ ircBotPlugins ircBotState''') ircBotState'''
  where
    runLoop :: IrcBotState -> IO (BotStatus, IrcBotState)
    runLoop botState = do
        (status, botState') <- (runStateT ircBotCore botState) `catches` [ Handler (\ (_ :: IOException) -> return (BotExit, botState))
                                                                         , Handler (\ (_ :: AsyncException) -> return (BotExit, botState)) ]
        case status of
            BotContinue -> runLoop botState'
            _           -> return (status, botState')

-- | Runs the IrcBot's reader loop
ircBotReader :: Handle -> Chan IrcBotMsg -> ThreadId -> IO ()
ircBotReader handle chan fatherThreadId = forever $ do
    str <- (hGetLine handle) `catch` handleIOException
    let msg = parseIrcMsg str
    case msg of
        Right msg' -> writeChan chan (InIrcMsg msg')
        _          -> return ()
  where
    handleIOException :: IOException -> IO (String)
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

-- | Initialize the bot's server connection
initBotServerConnection :: IrcConfig -> IrcBot ()
initBotServerConnection config = do
    ircBot <- get
    let ircServerState = ircBotServerState ircBot
    ircServerState' <- execStateT (initServerConnection config) ircServerState
    put $ ircBot { ircBotServerState = ircServerState' }

-- | Run the IrcBot's main loop
ircBotCore :: IrcBot (BotStatus)
ircBotCore = do
    chan <- gets ircBotChan
    msg  <- liftIO $ readChan chan
    case msg of
        InIrcMsg inIrcMsg   -> dispatchMessage $ InIrcMsg inIrcMsg
        OutIrcMsg outIrcMsg -> sendThisMessage outIrcMsg
        IntIrcCmd intIrcCmd -> processInternalCommand $ IntIrcCmd intIrcCmd
  where
    sendThisMessage :: IrcMsg -> IrcBot (BotStatus)
    sendThisMessage outputMsg = do
        let str = serializeIrcMsg outputMsg
        handle <- gets ircBotHandle
        liftIO $ hPutStr handle (str ++ "\r\n")
        return BotContinue

-- | Dispatches an input message
dispatchMessage :: IrcBotMsg -> IrcBot (BotStatus)
dispatchMessage (InIrcMsg inIrcMsg) = do
    bot <- get
    let config  = ircBotConfig bot
        plugins = ircBotPlugins bot
        cmds    = ircBotCommands bot
    if isPluginCommand config
      then
        let getMsgContent = unwords . tail $ ircMsgParameters inIrcMsg
            key         = tail . head $ words getMsgContent
            pluginNames = fromMaybe [] $ M.lookup key cmds
            plugins'    = fromMaybe [] $ mapM (flip M.lookup plugins) pluginNames
            sendRunCommand cmd plugin =  sendToPlugin (IntIrcCmd $ IrcCmd "RUN" "CORE" (ircPluginName plugin) cmd inIrcMsg) plugin
        in mapM_ (sendRunCommand (tail getMsgContent) . first) plugins'
      else
        mapM_ (sendToPlugin (InIrcMsg inIrcMsg) . first) (M.elems plugins)
    return BotContinue
  where
    isPluginCommand config = and [ ircMsgCommand inIrcMsg == "PRIVMSG", prefix == ircConfigCommandPrefix config ]
    prefix | length msgWords >= 1 = head . head $ msgWords
               | otherwise = ' '
      where
        msgWords = tail $ ircMsgParameters inIrcMsg
dispatchMessage _ = return (BotContinue)

