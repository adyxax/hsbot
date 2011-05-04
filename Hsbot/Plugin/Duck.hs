module Hsbot.Plugin.Duck
    ( duck
    , theDuck
    ) where

import Control.Concurrent.Chan ()
import qualified Data.List as L
import Control.Monad.State
import qualified Network.IRC as IRC
import Prelude hiding (catch)

import Hsbot.Message
import Hsbot.Types

duck :: PluginId
duck = PluginId
    { pluginName = "duck"
    , pluginEp   = theDuck }

-- | The IrcPlugin monad main function
theDuck :: Plugin (Env IO) ()
theDuck = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = answerMsg msg . concat . isThereADuckToKillInThere . concat $ IRC.msg_params msg
        | otherwise = return ()
    eval _ = return ()

isThereADuckToKillInThere :: String -> [String]
isThereADuckToKillInThere = concatMap (\y -> map (\x -> if x `L.isInfixOf` y then "PAN! " else "") ducks) . words

ducks :: [String]
ducks = ["\\_o<", "\\_O<", "\\_o>", "\\_O>", "\\o<", "\\O<", "\\o>", "\\O>",
         ">o_/", ">O_/", "<o_/", "<O_/", ">o/", ">O/", "<o/", "<O/" ]

