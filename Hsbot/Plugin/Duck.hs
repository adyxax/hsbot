{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
-- | This module is an IRC plugin that generates and kills ducks
module Hsbot.Plugin.Duck
    ( DuckArgs (..)
    , defaultDuckArgs
    , duck
    , theDuck
    ) where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Ord ()
import Data.SafeCopy
import Data.Typeable
import qualified Network.IRC as IRC
import System.Environment.XDG.BaseDir
import System.Random

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

-- The statistics database
data StatDB = StatDB
    { nickStats :: M.Map String Int
    } deriving (Show, Typeable)

-- | Statistics database initial state
emptyStatDB :: StatDB
emptyStatDB = StatDB { nickStats = M.empty }

$(deriveSafeCopy 0 'base ''StatDB)

-- | Statistics database transactions
updateScore :: String -> Int -> Update StatDB ()
updateScore sender score = do
    statDB <- get
    let stats = nickStats statDB
        stat = fromMaybe 0 $ M.lookup sender stats
    put statDB { nickStats = M.insert sender (stat + score) stats }

getDuckStats :: Query StatDB StatDB
getDuckStats = ask

$(makeAcidic ''StatDB ['getDuckStats, 'updateScore])

-- | The duck plugin identity
duck :: PluginId
duck = PluginId
    { pluginName = "duck"
    , pluginEp   = theDuck defaultDuckArgs }

data DuckArgs = DuckArgs
    { duckChannel :: String
    , duckDbName  :: String
    , duckFreq    :: Int }

defaultDuckArgs :: DuckArgs
defaultDuckArgs = DuckArgs { duckChannel = "", duckDbName = "duckDB", duckFreq = 7200 }

-- | An IRC plugin that generates and kills ducks
theDuck :: DuckArgs -> Plugin (Env IO) ()
theDuck (DuckArgs channel dbName seconds) = do
    baseDir <- liftIO $ System.Environment.XDG.BaseDir.getUserDataDir "hsbot"
    statDB <- liftIO $ openLocalStateFrom (baseDir ++ "/" ++ dbName ++ "/") emptyStatDB
    ducksMVar <- liftIO newEmptyMVar
    timeMVar <- liftIO $ newMVar seconds
    duckSpawner channel seconds ducksMVar
    forever $ readMsg >>= eval statDB ducksMVar timeMVar
  where
    eval :: AcidState StatDB -> MVar Int -> MVar Int -> Message -> Plugin (Env IO) ()
    eval statDB ducksMVar timeMVar (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = do
            -- First we kill the ducks that we find in the message
            let kills = howManyDucksInThere . concat $ IRC.msg_params msg
            when (kills /= "") $ answerMsg msg kills
            when (getDestination msg == channel) $ do
                -- Then we check if someone shot some duck
                let shots = howManyBulletFiredInThere . concat $ IRC.msg_params msg
                when (shots > 0) $ do
                    empty <- liftIO $ isEmptyMVar ducksMVar
                    ducksWaitingForDeath <- if empty then return 0
                                                     else liftIO $ modifyMVar ducksMVar (\x -> return (x - shots, x))
                    _ <- liftIO . update statDB . UpdateScore (getSender msg) $ computeScore ducksWaitingForDeath shots
                    when ((ducksWaitingForDeath > 0) && (shots >= ducksWaitingForDeath)) $ do
                        _ <- liftIO $ takeMVar ducksMVar
                        time <- liftIO $ readMVar timeMVar
                        duckSpawner channel time ducksMVar
                    return ()
                -- Finally we check if we received some command
                cmdArgs <- lift $ getCommand msg
                case cmdArgs of
                    "duck":"freq":time:_ -> case reads time :: [(Int, String)] of
                                                (secs,_):_ -> liftIO $ modifyMVar_ timeMVar (\_ -> return secs)
                                                _ -> answerMsg msg "Invalid time value."
                    "duck":"freq":_ -> answerMsg msg $ "You must provide an amount of seconds the bot should wait before spawning "
                                                     ++ "new ducks after the end of a round."
                    "duck":"stat":_ -> liftIO (query statDB GetDuckStats) >>= printDuckStats channel
                    "duck":_ -> answerMsg msg "Invalid duck command."
                    _ -> return ()
        | otherwise = return ()
    eval _ _ _ _ = return ()
    computeScore :: Int -> Int -> Int
    computeScore ducksWaitingForDeath shots
        | shots < ducksWaitingForDeath = shots - 1
        | shots == ducksWaitingForDeath = shots + 1
        | otherwise = negate shots

-- | Spawns ducks on a channel, just waiting to be shot
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

-- | Tels how many shots we can hear from this string
howManyBulletFiredInThere :: String -> Int
howManyBulletFiredInThere = sum . concatMap (\y -> map (\x -> if x `L.isInfixOf` y then 1 else 0) bangs) . words

-- | Shoot as many times there are ducks in the provided string
howManyDucksInThere :: String -> String
howManyDucksInThere = concat . concatMap (\y -> map (\x -> if x `L.isInfixOf` y then "PAN! " else "") ducks) . words

-- | Output a string made of the specified number of random ducks
someRandomDuck :: IO String
someRandomDuck = do
    whatDuck <- getStdRandom $ randomR (0,length ducks - 1)
    return $ ducks !! whatDuck ++ "    "

-- | There are many ways to hide as a duck, this function tries to cover most of them
ducks :: [String]
ducks = [ x : y : z | x <- ">=", y <- face, z <- ["__/", "_/"] ]
     ++ [ L.reverse $ x : y : z | x <- "<=", y <- face, z <- ["__\\", "_\\"] ]
  where
    face :: String
    face = "oO°@©®ð*òôóø⊕ΩꙫꙩꙨ◔ȯõ⁰"

-- | Weapons can have different noises
bangs :: [String]
bangs = [ "PAN", "PAN!" ]

-- | Nicely prints duck statistics
printDuckStats :: String -> StatDB -> Plugin (Env IO) ()
printDuckStats channel statDB = do
    sendLine "Duck slaughter simulator - Hall of Fame"
    mapM_ (sendLine . buildLine) . reverse . L.sortBy scoreSort $ M.toList (nickStats statDB)
  where
    scoreSort :: (String, Int) -> (String, Int) -> Ordering
    scoreSort (_, s1) (_,s2)
      | s1 < s2 = LT
      | s1 > s2 = GT
      | otherwise = EQ
    buildLine :: (String, Int) -> String
    buildLine (nick, score) = concat [ nick,  ": ", show score ]
    sendLine :: String -> Plugin (Env IO) ()
    sendLine msg = writeMsg . OutgoingMsg $ IRC.Message Nothing "PRIVMSG" [channel, msg]

