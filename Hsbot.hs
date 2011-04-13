module Hsbot
    ( hsbot
    ) where

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch
import Control.Monad.Reader
import System.Log.Logger

import Hsbot.Config
import Hsbot.Core
import Hsbot.Types

startHsbot :: Config -> IO ()
startHsbot config = do
    -- checking for configuration file compilation error
    case configErrors config of
         Nothing -> return ()
         Just em -> putStrLn $ "Error: " ++ em
    -- initialization
    infoM "Hsbot" "Bot initializations"
    hsbotEnv <- initHsbot config
    -- main stuff
    infoM "Hsbot" "Bot core starting"
    status <- runReaderT runHsbot hsbotEnv
    infoM "Hsbot" $ "Bot core exited with status " ++ (show status)
    -- Handling exit signal
    case status of
         BotContinue -> startHsbot config -- TODO do something not so dumb about starting over
         BotExit -> runReaderT terminateHsbot hsbotEnv
         BotReload -> relaunchMaster Nothing -- TODO relaunchWithTextState (state { stateConfig = config }) Nothing, add a flag that prevent spawning the sockets again
         BotRestart -> relaunchMaster Nothing -- TODO relaunch and kill sockets

hsbot :: Config -> IO ()
hsbot = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "hsbot"
    , Dyre.realMain    = startHsbot
    , Dyre.showError   = (\config err -> config { configErrors = Just err }) }

