module Hsbot.Config
    ( BotConfig (..)
    ) where

import Hsbot.Irc.Config

-- | Configuration data type
data BotConfig = IrcBotConfig IrcConfig

