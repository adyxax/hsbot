module Hsbot.IRC
    ( IrcInput(..)
    , IrcOutput(..)
    , parseIrcMsg
    )where

import qualified Network.IRC as Irc
import System.IO (Handle)

import Hsbot.Core

type User    = String
type Channel = String
type Command = String
type Args    = [String]

-- | Information from IRC
data IrcInput = Cmd  User Channel (Command, Maybe String) -- a regular command
              | Line User Channel String                  -- a normal line of little significance
              | Err String                                -- an error occured in parsing
    deriving (Eq,Show)

-- | Data that can go over the remote channel
data IrcOutput = Str String              -- a regular string
               | Quit (Server, Handle)   -- a quit message from a server
               | Join (Server, Channel)  -- joined a channel
               | Part (Server, Channel)  -- parted the channel
               | Reboot                  -- reboot message sent
               | Nil                     -- signifies thread death, only happens after reboot
    deriving (Eq,Show)

parseIrcMsg :: String -> IrcInput
parseIrcMsg _ = Err "Parsing not yet implemented"

