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

import Hsbot.Config
import Hsbot.Message
import Hsbot.Plugin
import Hsbot.Types

-- | Bot's main entry point
hsbot :: Config -> IO ()
hsbot config = do
    startTime <- getCurrentTime
    putStrLn "[Hsbot] Opening communication channel... "
    chan <- newChan :: IO (Chan BotMsg)
    mvar <- newMVar M.empty :: IO (MVar BotResumeData)
    putStrLn "[Hsbot] Spawning IrcBot plugins... "
    botState <- execStateT spawnIrcPlugins BotState { botStartTime  = startTime
                                                    , botPlugins    = M.empty
                                                    , botChan       = chan
                                                    , botConfig     = config
                                                    , botResumeData = mvar }
    putStrLn "[Hsbot] Entering main loop... "
    (status, botState') <- runLoop botState
    resumeData <- takeMVar mvar
    if status == BotReboot
      then resumeHsbot botState' resumeData
      else return ()
  where
    runLoop :: BotState -> IO (BotStatus, BotState)
    runLoop botState = do
        (status, botState') <- (runStateT botCore botState) `catch` (\(_ :: IOException) -> return (BotExit, botState))
        case status of
            BotContinue -> runLoop botState'
            _           -> return (status, botState')

resumeHsbot :: BotState -> BotResumeData -> IO ()
resumeHsbot botState resumeData = do
    print resumeData

-- | Run the bot main loop
botCore :: Bot (BotStatus)
botCore = do
    chan <- gets botChan
    msg  <- liftIO $ readChan chan
    case msg of
        InMsg  _      -> return BotContinue
        OutMsg _      -> return BotContinue
        IntMsg intMsg -> processInternalMessage $ IntMsg intMsg
        UpdMsg updMsg -> processUpdateMessage updMsg

-- | Process an update command
processUpdateMessage :: ResumeMsg -> Bot (BotStatus)
processUpdateMessage msg = do
    resumeData <- gets botResumeData
    let from  = resMsgFrom msg
        stuff = resMsgData msg
    liftIO $ modifyMVar_ resumeData (\oldData -> return $ M.insert from stuff oldData)
    return BotContinue

