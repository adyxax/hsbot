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
    mvar <- newMVar "" :: IO (MVar String)
    putStrLn "[Hsbot] Spawning IrcBot plugins... "
    botState <- execStateT spawnIrcPlugins BotState { botStartTime  = startTime
                                                    , botPlugins    = M.empty
                                                    , botChan       = chan
                                                    , botConfig     = config
                                                    , botMVar       = mvar
                                                    , botResumeData = M.empty }
    putStrLn "[Hsbot] Entering main loop... "
    (reboot, botState') <- (runStateT botLoop botState) `catch` (\(_ :: IOException) -> return (False, botState))
    resumeData <- takeMVar mvar
    if reboot
      then resumeHsbot botState' resumeData
      else return ()

resumeHsbot :: BotState -> String -> IO ()
resumeHsbot botState resumeData = do
    print resumeData

-- | Run the bot main loop
botLoop :: Bot (Bool)
botLoop = do
    chan <- gets botChan
    msg  <- liftIO $ readChan chan
    case msg of
        InMsg  _      -> botLoop
        OutMsg _      -> botLoop
        IntMsg intMsg -> do
            reboot <- processInternalMessage $ IntMsg intMsg
            reportUpdate
            if not reboot
              then botLoop
              else return (True)

-- | Reports an update to the master bot
reportUpdate :: Bot ()
reportUpdate = do
    bot <- get
    let mvar  = botMVar bot
        stuff = show $ botResumeData bot
    _ <- liftIO $ swapMVar mvar stuff
    return ()

