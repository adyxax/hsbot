module Hsbot.Utils
    ( inColor
    , sendstr
    , trace
    , traceM
    ) where

import Control.Monad.State
import Data.List
import System.IO

import Hsbot.Types

-- |Wrap a string with ANSI escape sequences.
inColor :: String -> [Int] -> String
inColor str vals = "\ESC[" ++ valstr ++ "m" ++ str ++ "\ESC[0m"
    where valstr = concat $ intersperse ";" $ map show vals

-- | Sends a string over handle
sendstr :: String -> IrcBot ()
sendstr str = do
    handle <- gets botHandle
    traceM $ inColor ("--> " ++ str) [33]
    liftIO $ hPutStr handle (str ++ "\r\n")

-- | Log a message string
trace :: String -> IO ()
trace msg = putStrLn msg

-- | Log a message string
traceM :: String -> IrcBot ()
traceM msg = liftIO $ putStrLn msg

-----------------
-- | Helpers | --
-----------------
-- sendRegister

