module Hsbot.Core
    ( hsbot
    ) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M
import Data.Time
import Prelude hiding (catch)
import System.IO()
import System.Posix.Signals

import Hsbot.Config
import Hsbot.Message
import Hsbot.Plugin
import Hsbot.Types

-- | Bot's main entry point
hsbot :: [BotConfig] -> IO ()
hsbot config = do
    startTime <- getCurrentTime
    putStrLn "[Hsbot] Opening communication channel... "
    chan <- newChan :: IO (Chan BotMsg)
    mvar <- newMVar M.empty :: IO (MVar BotResumeData)
    putStrLn "[Hsbot] Installing signal handlers... "
    _ <- installHandler sigHUP (Catch $ sigHupHandler chan) Nothing
    _ <- installHandler sigTERM (Catch $ sigTermHandler chan) Nothing
    putStrLn "[Hsbot] Spawning IrcBot plugins... "
    botState <- execStateT spawnPlugins BotState { botStartTime  = startTime
                                                 , botPlugins    = M.empty
                                                 , botChan       = chan
                                                 , botConfig     = config
                                                 , botResumeData = mvar }
    putStrLn "[Hsbot] Entering main loop... "
    (status, botState') <- runLoop botState
    putStrLn "[Hsbot] Killing active plugins... "
    resumeData <- takeMVar mvar
    evalStateT (mapM_ killPlugin $ M.keys resumeData) botState'
    if status == BotReboot
      then resumeHsbot resumeData
      else return ()
  where
    runLoop :: BotState -> IO (BotStatus, BotState)
    runLoop botState = do
        (status, botState') <- (runStateT botCore botState) `catches` [ Handler (\ (_ :: IOException) -> return (BotExit, botState))
                                                                      , Handler (\ (_ :: AsyncException) -> return (BotExit, botState)) ]
        case status of
            BotContinue -> runLoop botState'
            _           -> return (status, botState')

resumeHsbot :: BotResumeData -> IO ()
resumeHsbot resumeData = do
    print resumeData

-- | Run the bot main loop
botCore :: Bot (BotStatus)
botCore = do
    chan <- gets botChan
    msg  <- liftIO $ readChan chan
    case msg of
        IntMsg intMsg -> processInternalMessage intMsg
        UpdMsg updMsg -> processUpdateMessage updMsg
        RebMsg rebMsg -> processRebootMessage rebMsg
        ExiMsg exiMsg -> processExitMessage exiMsg

-- | Process an update command
processUpdateMessage :: ResumeMsg -> Bot (BotStatus)
processUpdateMessage msg = do
    resumeData <- gets botResumeData
    let from  = resMsgFrom msg
        stuff = resMsgData msg
    liftIO $ modifyMVar_ resumeData (\oldData -> return $ M.insert from stuff oldData)
    return BotContinue

-- | signals handlers
sigHupHandler :: Chan BotMsg -> IO ()
sigHupHandler chan = writeChan chan $ RebMsg RebootMsg { rebMsgFrom = "HUP handler" }

-- | signals handlers
sigTermHandler :: Chan BotMsg -> IO ()
sigTermHandler chan = writeChan chan $ ExiMsg ExitMsg { exiMsgFrom = "TERM handler" }

