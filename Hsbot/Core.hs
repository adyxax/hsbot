module Hsbot.Core
    ( hsbot
    ) where

import Control.Concurrent
import Control.Concurrent.Chan ()
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
    putStrLn "[Hsbot] Spawning bot state manager... "
    processUpdateChan <- newChan :: IO (Chan String)
    reportUpdateChan  <- newChan :: IO (Chan String)
    updaterThreadId <- forkIO $ readUpdates processUpdateChan reportUpdateChan ""
    putStrLn "[Hsbot] Spawning IrcBot plugins... "
    botState <- execStateT spawnIrcPlugins BotState { botStartTime  = startTime
                                                    , botPlugins    = M.empty
                                                    , botChan       = chan
                                                    , botConfig     = config
                                                    , botUpdateChan = processUpdateChan
                                                    , botResumeData = M.empty }
    putStrLn "[Hsbot] Entering main loop... "
    _ <- (execStateT botLoop botState) `catch` (\(_ :: IOException) -> return botState)
    killThread updaterThreadId
    resumeData <- readChan reportUpdateChan
    print resumeData
    return ()

-- | Run the bot main loop
botLoop :: Bot ()
botLoop = forever $ do
    chan <- gets botChan
    msg  <- liftIO $ readChan chan
    case msg of
        InMsg  _      -> return ()
        OutMsg _      -> return ()
        IntMsg intMsg -> do
            processInternalMessage $ IntMsg intMsg
            reportUpdate

-- | Reports an update to the master bot
reportUpdate :: Bot ()
reportUpdate = do
    bot <- get
    let updateChan = botUpdateChan bot
        stuff      = show $ botResumeData bot
    liftIO $ writeChan updateChan stuff

-- | Runs bot updates' manager thread
readUpdates :: Chan String -> Chan String -> String -> IO ()
readUpdates processChan reportChan resumeData = do
    resumeData' <- (readChan processChan) `catch` handleException
    readUpdates processChan reportChan resumeData'
  where
    handleException :: AsyncException -> IO (String)
    handleException _ = do
        writeChan reportChan resumeData
        myId <- myThreadId
        killThread myId
        return ""

