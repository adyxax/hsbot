module Hsbot.Message
    ( BotMsg (..)
    , Msg (..)
    , processInternalMessage
    ) where

import Hsbot.PluginUtils

-- | A hsbot message
data Msg = Msg
    { msgType :: String -- the message type
    , msgFrom :: String -- who issues it
    , msgTo   :: String -- who it is destinated to
    , msgCmd  :: String -- the message to be transfered
    } deriving (Show)

data BotMsg = InMsg Msg | OutMsg Msg | IntMsg Msg deriving (Show)

