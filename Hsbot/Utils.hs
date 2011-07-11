module Hsbot.Utils
    ( addThreadIdToQuitMVar
    , delThreadIdFromQuitMVar
    , initTLSEnv
    , sendStr
    , setGlobalQuitMVar
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.List
import Network.TLS
import System.IO

import Hsbot.Types

-- utility functions
addThreadIdToQuitMVar :: ThreadId -> Env IO ()
addThreadIdToQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (\l -> return $ thrId:l)

delThreadIdFromQuitMVar :: ThreadId -> Env IO ()
delThreadIdFromQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (return . delete thrId)

setGlobalQuitMVar :: BotStatus -> Env IO ()
setGlobalQuitMVar status = do
    quitMv <- asks envQuitMv
    liftIO $ putMVar quitMv status

-- Helpers
sendStr :: Handle -> Maybe TLSCtx -> String -> IO ()
sendStr _ (Just ctx) msg = sendData ctx . L.fromString $ msg ++ "\r\n"
sendStr handle Nothing msg = hPutStrLn handle $ msg ++ "\r\n"

-- TLS utils
initTLSEnv :: TLSConfig -> IO TLSParams
initTLSEnv ssl = do
    let versions = sslVersions ssl
        ciphers  = sslCiphers ssl
    return $ defaultParams { pAllowedVersions = versions
                           , pCiphers = ciphers }

