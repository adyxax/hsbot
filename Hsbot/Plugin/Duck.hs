-- | This module is an IRC plugin that generates and kills ducks
module Hsbot.Plugin.Duck
    ( duck
    , theDuck
    ) where

import Control.Concurrent.Chan ()
import qualified Data.List as L
import Control.Monad.Reader
import qualified Network.IRC as IRC
import Prelude hiding (catch)

import Hsbot.Message
import Hsbot.Types

-- | The duck plugin identity
duck :: PluginId
duck = PluginId
    { pluginName = "duck"
    , pluginEp   = theDuck }

-- | An IRC plugin that generates and kills ducks
theDuck :: Plugin (Env IO) ()
theDuck = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = answerMsg msg . isThereADuckToKillInThere . concat $ IRC.msg_params msg
        | otherwise = return ()
    eval _ = return ()

-- | Shoot as many times are there are ducks in the initial string
isThereADuckToKillInThere :: String -> String
isThereADuckToKillInThere = concat . concatMap (\y -> map (\x -> if x `L.isInfixOf` y then "PAN! " else "") ducks) . words

-- | There are many ways to hide as a duck, this function tries to cover most of them
ducks :: [String]
ducks = [ x : y : z | x <- nose, y <- face, z <- ["__/", "_/", "/"] ]
     ++ [ L.reverse $ x : y : z | x <- nose, y <- face, z <- ["__\\", "_\\", "\\"] ]
  where
    nose :: [Char]
    nose = "<>="
    face :: [Char]
    face = "oO°@©®ð*òôóø"

