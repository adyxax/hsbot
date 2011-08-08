module Hsbot
    ( hsbot
    ) where

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch
import Control.Monad.Reader
import qualified Data.Map as M
import System.Log.Logger
import qualified Network.IRC as IRC

import Hsbot.Core
import Hsbot.Types
import Hsbot.Utils

data State = State (M.Map String String) deriving (Read, Show)

startHsbot :: Config -> IO ()
startHsbot config = do
    (State buffer) <- restoreTextState $ State M.empty
    -- checking for configuration file compilation error
    case configErrors config of
         Nothing -> return ()
         Just em -> putStrLn $ "Error: " ++ em
    -- initialization
    infoM "Hsbot" "Bot initializations"
    hsbotEnv <- initHsbot config
    -- Handle previous exit state if it exists
    die_msgs <- case M.lookup "die_msg" buffer of
        Just dieMsg -> case reads dieMsg :: [(BotStatus, String)] of
            (status, _):_ -> case status of
                BotReload reason -> return ["hsbot reloaded, reason : " ++ reason]
                BotRestart (reason, Just info) -> return ["hsbot restarted, readon : " ++ reason, "additional information: " ++ info]
                BotRestart (reason, Nothing) -> return ["hsbot restarted, readon : " ++ reason]
                BotExit -> return []
            _ -> return ["hsbot die_msg parsing error, this should not happen"]
        Nothing -> return []
    let connhdl  = envHandle hsbotEnv
        tlsCtx   = envTLSCtx hsbotEnv
        channels = configChannels config
    mapM_ (infoM "Hsbot") die_msgs
    mapM_ (\msg -> mapM_ (\channel -> sendStr hsbotEnv connhdl tlsCtx . IRC.encode $ IRC.Message Nothing "PRIVMSG" [channel, msg]) channels) die_msgs
    -- main stuff
    infoM "Hsbot" "Bot core starting"
    status <- runReaderT runHsbot hsbotEnv
    infoM "Hsbot" $ "Bot core exited with status " ++ show status
    -- Handling exit signal
    case status of
         BotExit -> runReaderT terminateHsbot hsbotEnv
         BotReload reason -> do
             runReaderT terminateHsbot hsbotEnv
             relaunchWithTextState (M.singleton "die_msg" . show $ BotReload reason) Nothing  -- TODO find a way to properly implement that, then insert necessary information in this MVar
         BotRestart reason -> do
             runReaderT terminateHsbot hsbotEnv
             relaunchWithTextState (M.singleton "die_msg" . show $ BotRestart reason) Nothing

hsbot :: Config -> IO ()
hsbot = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "hsbot"
    , Dyre.realMain    = startHsbot
    , Dyre.showError   = \config err -> config { configErrors = Just err } }

