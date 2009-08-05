module Config
    ( config
    ) where

import Hsbot.Core

-- | Imported plugins goes there

-- | User server
kro = IrcServer
    { address        = "kro.corp"
    , port           = 6667
    , channels       = ["#geek", "#shbot"]
    , nickname       = "hsbot"
    , password       = ""
    , realname       = "The One True bot, with it's haskell soul."
    , administrators = ["julien"]
    }

-- | User configuration
config :: Config
config = Config
    { commandPrefixes = ['@']
    , ircServers         = [kro]
    }

