module Hsbot.Config
    ( Config(..)
    , defaultConfig
    ) where

import Hsbot.Irc.Config (IrcConfig)

-- | Configuration data type
data Config = Config
    { ircConfigs :: [IrcConfig]
    }

-- | User configuration
defaultConfig :: Config
defaultConfig = Config
    { ircConfigs = []
    }

