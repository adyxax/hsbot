Hsbot
=====

Hsbot is an IRC bot written in haskell.

Quick Start
===========

Configuration goes in $HOME/.config/hsbot/hsbot.hs, here is a minimal example :

    import Hsbot
    import Hsbot.Config
    import Hsbot.Plugin.Ping
    import Hsbot.Types
    import Hsbot.Utils

    main :: IO ()
    main = do
        hsbot defaultConfig { configAddress   = "irc.example.org"
                            , configTLS       = noSSL
                            , configPort      = PortNumber 1337
                            , configPassword  = Nothing
                            , configChannels  = ["#hsbot", "#geek"]
                            , configNicknames = ["hsbot"]
                            , configRealname  = "The One True bot, with its haskell soul."
                            , configPlugins   = [ ping ] }

Configuration Guide
===================

You can find all configuration options available in the Hsbot/Config file : use the source Luke! Anyway, here are some of the most common stuff you might need.

TLS
---

To connect with SSL/TLS use the following :

        hsbot defaultConfig { ...
                            , configTLS       = defaultTLSConfig { sslVerify = False }

Logging
-------

You can implement logging using the standard haskell library, for example :

    import System.Log.Logger
    ...
    updateGlobalLogger rootLoggerName (setLevel DEBUG)

Connecting to multiple servers
------------------------------

Just use the power of forkIO :

    import Control.Concurrent
    ...
    forkIO $ hsbot defaultConfig { ...

Plugins
=======

Admin
-----

The admin plugin allows you to remotely perform basic administration tasks. To use it you have to configure an access list and activate the plugin :

    import qualified Network.IRC as IRC
    import Hsbot.Plugin.Admin
    ...
    hsbot defaultConfig { ...
                        , configAccess    = [ AccessList { accessMask = IRC.NickName "adyxax" (Just "~adyxax") (Just "where-you-are-connecting-from")
                                                         , accessList = [Admin] } ]
                        , configPlugins   = [ admin, ping ]
                        , ...

Beware that access lists currently dont support regex, it is planned for a future version.

Duck
----

A shoot the duck game, featuring a lot of cute UTF-8 friendly ducks :

    import Hsbot.Plugin.Duck
    ...
    let dargs = defaultDuckArgs { duckChannel = "#geek", duckFreq = 7200 } }
    hsbot defaultConfig { ...
                        , configPlugins   = [ ping, duck { pluginEp = theDuck dargs } ]
                        , ...

Quote
-----

The quote module allows to keep sayings for posterity and legend :

    import Hsbot.Plugin.Quote
    ...
    hsbot defaultConfig { ...
                        , configPlugins   = [ ping, quote ]
                        , ...

