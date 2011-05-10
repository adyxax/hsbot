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
    , pluginEp   = theDuck "" 10 }

-- | An IRC plugin that generates and kills ducks
theDuck :: String -> Int -> Plugin (Env IO) ()
theDuck channel seconds = do
    ducksMVar <- liftIO newEmptyMVar
    duckSpawner channel seconds ducksMVar
    forever $ readMsg >>= eval ducksMVar
  where
    eval :: MVar Int -> Message -> Plugin (Env IO) ()
    eval ducksMVar (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = do
            -- First we kill the ducks that we find in the message
            let kills = howManyDucksInThere . concat $ IRC.msg_params msg
            when (kills /= "") $ answerMsg msg kills
            -- Then we check if someone shot some duck
            let shots = howManyBulletFiredInThere . concat $ IRC.msg_params msg
            noDucksToShot <- liftIO $ isEmptyMVar ducksMVar
            when (and [getDestination msg == channel, not noDucksToShot, shots > 0]) $ do
                -- TODO: score dead ducks (with a min (ducksWaitingForDeath, shots))
                ducksWaitingForDeath <- liftIO $ modifyMVar ducksMVar (\x -> return (x - shots, x))
                when (shots >= ducksWaitingForDeath) $ do
                    _ <- liftIO $ takeMVar ducksMVar
                    duckSpawner channel seconds ducksMVar
                    -- TODO : score lost bullets and round
                    return ()
            -- Finally we check if we received some command
            cmdArgs <- lift $ getCommand msg
            case cmdArgs of
                --"ducks":"stats":args -> TODO
                "ducks":_ -> answerMsg msg "Invalid duck command."
                _ -> return ()
        | otherwise = return ()
    eval _ _ = return ()

-- | Regularly spawns ducks on a channel, just waiting to be shot
duckSpawner :: String -> Int -> MVar Int -> Plugin (Env IO) ()
duckSpawner channel secs ducksMVar = do
    pEnv <- ask
    lift ask >>= liftIO . forkIO . runReaderT (runReaderT trueSpawner pEnv) >>= lift . addThreadIdToQuitMVar
  where
    trueSpawner :: Plugin (Env IO) ()
    trueSpawner = do
        rsecs <- liftIO . getStdRandom $ randomR (1,secs)
        liftIO $ threadDelay (1000000 * rsecs)
        nbDucks <- liftIO . getStdRandom $ randomR (1,4)
        thoseDucks <- liftIO $ replicateM nbDucks someRandomDuck
        liftIO $ putMVar ducksMVar nbDucks
        writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [channel, concat thoseDucks]
        liftIO myThreadId >>= lift . delThreadIdFromQuitMVar

-- | Shoot as many times are there are ducks in the initial string
howManyBulletFiredInThere :: String -> Int
howManyBulletFiredInThere = sum . concatMap (\y -> map (\x -> if x `L.isInfixOf` y then 1 else 0) bangs) . words

-- | Shoot as many times are there are ducks in the initial string
howManyDucksInThere :: String -> String
howManyDucksInThere = concat . concatMap (\y -> map (\x -> if x `L.isInfixOf` y then "PAN! " else "") ducks) . words

-- | Output a string made of the specified number of random ducks
someRandomDuck :: IO String
someRandomDuck = do
    whatDuck <- getStdRandom $ randomR (0,length ducks - 1)
    return $ ducks !! whatDuck ++ "    "

-- | There are many ways to hide as a duck, this function tries to cover most of them
ducks :: [String]
ducks = [ x : y : z | x <- nose, y <- face, z <- ["__/", "_/", "/"] ]
     ++ [ L.reverse $ x : y : z | x <- nose, y <- face, z <- ["__\\", "_\\", "\\"] ]
  where
    nose :: String
    nose = "<>="
    face :: String
    face = "oO°@©®ð*òôóø"

-- | Weapons can have different noises
bangs :: [String]
bangs = [ "PAN", "PAN!" ]

