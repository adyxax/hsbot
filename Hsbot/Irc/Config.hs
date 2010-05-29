module Hsbot.Irc.Config
    ( IrcConfig(..)
    , ircDefaultConfig
    ) where

import Network

-- | Configuration data type
data IrcConfig = IrcConfig
    { ircConfigName          :: String   -- The configuration name
    , ircConfigAddress       :: String   -- the server's address
    , ircConfigPort          :: PortID   -- the server's port
    , ircConfigChannels      :: [String] -- the Channels to join on start
    , ircConfigNickname      :: String   -- the hsbot's nickname
    , ircConfigPassword      :: String   -- the hsbot's password, optional
    , ircConfigRealname      :: String   -- the hsbot's real name, optional
    , ircConfigCommandPrefix :: Char     -- the prefix the ircbot will recognize as commands
    , ircConfigPlugins       :: [String] -- the ircPlugins to load
    }

-- | User configuration
ircDefaultConfig :: IrcConfig
ircDefaultConfig = IrcConfig
    { ircConfigName          = "irc-alocalhost"
    , ircConfigAddress       = "localhost"
    , ircConfigPort          = PortNumber 6667
    , ircConfigChannels      = ["#hsbot"]
    , ircConfigNickname      = "hsbot"
    , ircConfigPassword      = ""
    , ircConfigRealname      = "The One True bot, with it's haskell soul."
    , ircConfigCommandPrefix = '@'
    , ircConfigPlugins       = ["Ping"]
    }

