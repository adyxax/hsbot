module Hsbot.Core
    ( hsbot
    ) where

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M
import Data.Time
import Prelude hiding (catch)
import System.IO()

import Hsbot.Config
import Hsbot.Irc.Config
import Hsbot.Irc.Core
import Hsbot.Message
import Hsbot.Plugin

-- | The Bot monad
type Bot = StateT BotState IO

-- | An Hsbot state
data BotState = BotState
    { botStartTime :: UTCTime                              -- the bot's uptime
    , botPlugins   :: M.Map String (PluginState, ThreadId) -- Loaded plugins
    , botChan      :: Chan BotMsg                          -- The bot's communication channel
    , botConfig    :: Config                               -- the bot's starting config
    }

-- | Bot's main entry point
hsbot :: Config -> IO ()
hsbot config = do
    startTime <- getCurrentTime
    putStrLn "[Hsbot] Opening communication channel... "
    chan <- newChan :: IO (Chan BotMsg)
    putStrLn "[Hsbot] Spawning IrcBot plugins... "
    botState <- execStateT spawnIrcPlugins BotState { botStartTime = startTime
                                                    , botPlugins   = M.empty
                                                    , botChan      = chan
                                                    , botConfig    = config }
    putStrLn "[Hsbot] Entering main loop... "
    botState' <- (execStateT botLoop botState) `catch` (\(_ :: IOException) -> return botState)
    return ()

-- | Run the bot main loop
botLoop :: Bot ()
botLoop = forever $ do
    chan <- gets botChan
    msg <- liftIO $ readChan chan
    -- process messages
    return ()

-- | spawns IrcPlugins
spawnIrcPlugins :: Bot ()
spawnIrcPlugins = do
    config <- gets botConfig
    mapM_ (spawnIrcPlugin) (ircConfigs config)
  where
    spawnIrcPlugin :: IrcConfig -> Bot ()
    spawnIrcPlugin config = do
        bot <- get
        let chan  = botChan bot
        pchan <- liftIO (newChan :: IO (Chan BotMsg))
        threadId <- liftIO $ forkIO (startIrcbot config chan pchan)
        let plugin  = PluginState { pluginName    = ircConfigName config
                                  , pluginChan    = pchan
                                  , pluginHandles = M.empty }
            plugins = botPlugins bot
        put $ bot { botPlugins = M.insert (pluginName plugin) (plugin, threadId) plugins }

