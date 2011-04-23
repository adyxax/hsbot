module Hsbot.Message
    ( Message (..)
    ) where

import qualified Network.IRC as IRC

data Message = IncomingMsg IRC.Message
             | OutgoingMsg IRC.Message

