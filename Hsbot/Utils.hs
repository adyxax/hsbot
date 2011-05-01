module Hsbot.Utils
    ( addThreadIdToQuitMVar
    , delThreadIdFromQuitMVar
    , first
    , initTLSEnv
    , readCertificate
    , readPrivateKey
    , sendStr
    , setGlobalQuitMVar
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Crypto.Cipher.RSA as RSA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Certificate.KeyRSA as KeyRSA
import Data.Certificate.PEM
import Data.Certificate.X509
import Data.List
import Data.Maybe
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

first :: (a, b, c) -> a
first (a, _, _) = a

-- Helpers
sendStr :: Handle -> Maybe TLSCtx -> String -> IO ()
sendStr _ (Just ctx) msg = sendData ctx $ L.fromChunks [C.pack msg]
sendStr handle Nothing msg = hPutStrLn handle msg

-- TLS utils
initTLSEnv :: TLSConfig -> IO TLSParams
initTLSEnv ssl = do
    let certFile = sslCert ssl
        keyFile  = sslKey ssl
        versions = sslVersions ssl
        ciphers  = sslCiphers ssl
        verify   = sslVerify ssl
    -- TODO : exception on loading keys
    cert <- readCertificate certFile
    pk   <- readPrivateKey keyFile
    return $ defaultParams { pConnectVersion = TLS12
                           , pAllowedVersions = versions
                           , pCiphers = ciphers
                           , pWantClientCert = verify
                           , pCertificates = [(cert, Just pk)] }

readCertificate :: FilePath -> IO X509
readCertificate filepath = do
    content <- B.readFile filepath
    let certdata = fromMaybe (error "no valid certificate section") $ parsePEMCert content
        cert = case decodeCertificate $ L.fromChunks [certdata] of
            Left err -> error ("cannot decode certificate: " ++ err)
            Right x  -> x
    return cert

readPrivateKey :: FilePath -> IO PrivateKey
readPrivateKey filepath = do
    content <- B.readFile filepath
    let pkdata = case parsePEMKeyRSA content of
            Nothing -> error "no valid RSA key section"
            Just x  -> L.fromChunks [x]
    let pk = case KeyRSA.decodePrivate pkdata of
            Left err -> error ("cannot decode key: " ++ err)
            Right x  -> PrivRSA RSA.PrivateKey
                { RSA.private_sz   = fromIntegral $ KeyRSA.lenmodulus x
                , RSA.private_n    = KeyRSA.modulus x
                , RSA.private_d    = KeyRSA.private_exponant x
                , RSA.private_p    = KeyRSA.p1 x
                , RSA.private_q    = KeyRSA.p2 x
                , RSA.private_dP   = KeyRSA.exp1 x
                , RSA.private_dQ   = KeyRSA.exp2 x
                , RSA.private_qinv = KeyRSA.coef x
                }
    return pk

