module Config
    ( config
    , defaultPlugins
    ) where

import Network

import Hsbot.Types

-- | Imported plugins goes there
defaultPlugins :: [String]
defaultPlugins = [ "Ping" ]

-- | User server
kro :: IrcServer
kro = IrcServer
    { address        = "kro.corp"
    , port           = PortNumber 6667
    , channels       = ["#shbot"]
    , nickname       = "hsbot"
    , password       = ""
    , realname       = "The One True bot, with it's haskell soul."
    , administrators = ["julien"]
    }

-- | User configuration
config :: Config
config = Config
    { commandPrefix = '@'
    , ircServer     = kro
    }

