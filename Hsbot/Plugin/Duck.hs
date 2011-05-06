-- | This module is an IRC plugin that generates and kills ducks
module Hsbot.Plugin.Duck
    ( duck
    , theDuck
    ) where

import Control.Concurrent
import qualified Data.List as L
import Control.Monad.Reader
import qualified Network.IRC as IRC
import Prelude hiding (catch)
import System.Random

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

-- | The duck plugin identity
duck :: PluginId
duck = PluginId
    { pluginName = "duck"
    , pluginEp   = theDuck "" 11 }

-- | An IRC plugin that generates and kills ducks
theDuck :: String -> Int -> Plugin (Env IO) ()
theDuck channel seconds = do
    env <- lift ask
    pEnv <- ask
    secondsMVar <- liftIO $ newMVar seconds
    killMVar <- liftIO newEmptyMVar
    (liftIO . forkIO $ runReaderT (runReaderT (duckSpawner channel secondsMVar killMVar) pEnv) env) >>= lift . addThreadIdToQuitMVar
    forever $ readMsg >>= eval
  where
    eval :: Message -> Plugin (Env IO) ()
    eval (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = answerMsg msg . isThereADuckToKillInThere . concat $ IRC.msg_params msg
        | otherwise = return ()
    eval _ = return ()

-- | Regularly spawns ducks on a channel, just waiting to be shot
duckSpawner :: String -> MVar Int -> MVar Int -> Plugin (Env IO) ()
duckSpawner channel secondsMVar killMVar = forever $ do
    nbDucks <- liftIO . getStdRandom $ randomR (1,4)
    liftIO $ putMVar killMVar nbDucks
    thoseDucks <- liftIO $ someRandomDucks nbDucks ""
    writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [channel, thoseDucks]
    secs <- liftIO $ readMVar secondsMVar
    liftIO $ threadDelay (3000000 * secs)

-- | Shoot as many times are there are ducks in the initial string
isThereADuckToKillInThere :: String -> String
isThereADuckToKillInThere = concat . concatMap (\y -> map (\x -> if x `L.isInfixOf` y then "PAN! " else "") ducks) . words

someRandomDucks :: Int -> String -> IO String
someRandomDucks 0 theDucks = return theDucks
someRandomDucks nbDucks theDucks = do
    whatDuck <- getStdRandom $ randomR (1,length ducks)
    let thisDuck = ducks !! whatDuck
    someRandomDucks (nbDucks -1 ) $ concat [thisDuck, " ", theDucks]

-- | There are many ways to hide as a duck, this function tries to cover most of them
ducks :: [String]
ducks = [ x : y : z | x <- nose, y <- face, z <- ["__/", "_/", "/"] ]
     ++ [ L.reverse $ x : y : z | x <- nose, y <- face, z <- ["__\\", "_\\", "\\"] ]
  where
    nose :: String
    nose = "<>="
    face :: String
    face = "oO°@©®ð*òôóø"

