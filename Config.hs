module Config
    ( config
    , defaultPlugins
    ) where

import Network

import Hsbot.Types

-- | Imported plugins goes there
defaultPlugins :: [String]
defaultPlugins = [ "Ping", "Core" ]

-- | User server
localhost :: IrcServer
localhost = IrcServer
    { serverAddress  = "localhost"
    , serverPort     = PortNumber 6667
    , joinChannels   = ["#shbot"]
    , nickname       = "hsbot"
    , password       = ""
    , realname       = "The One True bot, with it's haskell soul."
    , administrators = ["julien"]
    }

-- | User configuration
config :: Config
config = Config
    { commandPrefix = '@'
    , ircServer     = localhost
    }

