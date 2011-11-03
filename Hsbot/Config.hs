module Hsbot.Config
    ( defaultConfig
    , defaultTLSConfig
    , noSSL
    ) where

import Network
import Network.TLS
import Network.TLS.Extra

import Hsbot.Types

defaultConfig :: Config
defaultConfig = Config
    { configErrors    = Nothing
    , configTLS       = noSSL
    , configAddress   = "localhost"
    , configPort      = PortNumber 6667
    , configAccess    = []
    , configChannels  = ["#hsbot"]
    , configNicknames = ["hsbot"]
    , configRealname  = "The One True bot, with it's haskell soul."
    , configPlugins   = [] }

defaultTLSConfig :: TLSConfig
defaultTLSConfig = TLSConfig
    { sslOn       = True
    , sslVersions = [SSL3, TLS10, TLS11, TLS12]
    , sslCiphers  = [ cipher_AES128_SHA1
                    , cipher_AES256_SHA1
                    , cipher_RC4_128_MD5
                    , cipher_RC4_128_SHA1
                    , cipher_AES256_SHA1
                    , cipher_AES128_SHA256
                    , cipher_AES256_SHA256 ]
    , sslVerify   = True
    , sslLogging  = defaultLogging }

noSSL :: TLSConfig
noSSL = defaultTLSConfig { sslOn = False }

