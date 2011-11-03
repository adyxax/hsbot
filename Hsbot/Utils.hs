module Hsbot.Utils
    ( addThreadIdToQuitMVar
    , delThreadIdFromQuitMVar
    , hasAccess
    , initTLSEnv
    , sendStr
    , setGlobalQuitMVar
    ) where

import Control.Concurrent
import Control.Exception (IOException, catch)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.List as L
import qualified Network.IRC as IRC
import Network.TLS
import Prelude hiding (catch)
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
    liftIO $ modifyMVar_ threadIdsMv (return . L.delete thrId)

setGlobalQuitMVar :: BotStatus -> Env IO ()
setGlobalQuitMVar status = do
    quitMv <- asks envQuitMv
    liftIO $ putMVar quitMv status

-- Access rights
hasAccess :: Maybe IRC.Prefix -> AccessRight -> Env IO Bool
hasAccess Nothing _ = return False
hasAccess (Just mask) right =
    asks envBotState >>= liftIO . readMVar >>= evalStateT (fmap (any accessMatch) (gets botAccess))
  where
    accessMatch :: AccessList -> Bool
    accessMatch (AccessList amask arights)
      | mask == amask = (Admin `L.elem` arights) || (right `L.elem` arights)
      | otherwise = False

-- Helpers
sendStr :: BotEnv -> Handle -> Maybe (TLSCtx Handle) -> String -> IO ()
sendStr env _ (Just ctx) msg = sendData ctx (L.fromString $ msg ++ "\r\n") `catch` handleIOException env ("sendStr " ++ msg)
sendStr env handle Nothing msg = hPutStrLn handle (msg ++ "\r\n") `catch` handleIOException env ("sendStr " ++ msg)

handleIOException :: BotEnv -> String -> IOException -> IO ()
handleIOException env msg ioException = do
    runReaderT (setGlobalQuitMVar $ BotRestart (show ioException, Just msg)) env
    myId <- myThreadId
    killThread myId
    return ()

-- TLS utils
initTLSEnv :: TLSConfig -> IO TLSParams
initTLSEnv ssl = do
    let versions = sslVersions ssl
        ciphers  = sslCiphers ssl
    return $ defaultParams { pAllowedVersions = versions
                           , pCiphers = ciphers }

