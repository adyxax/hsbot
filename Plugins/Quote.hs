module Plugins.Quote
    ( mainQuote
    ) where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.State
import Prelude hiding (catch)
import System.Time (ClockTime)

import Hsbot.IRCPlugin
import Hsbot.Types
import Hsbot.Utils

-- | A quote object
data Quote = Quote
    { quoter    :: String
    , quote     :: [String]
    , quoteTime :: ClockTime
    , votes     :: Int
    } deriving (Show)

-- | A QuoteBot state
type QuoteDB = [Quote]

-- | The QuoteBot monad
type QuoteBot a = StateT QuoteDB IO a

-- | The plugin's main entry point
mainQuote :: Chan BotMsg -> Chan BotMsg -> IO ()
mainQuote serverChan chan = do
    let plugin = PluginState "Quote" serverChan chan
    evalStateT (mapM_ sendRegisterCommand ["quote"]) plugin
    _ <- (evalStateT (run quoteBot) plugin) `catch` (\(_ :: AsyncException) -> return quoteBot)
    evalStateT (mapM_ sendUnregisterCommand ["quote"]) plugin

-- | The IrcPlugin monad main function
run :: IrcPlugin ()
run = forever $ do
    msg <- readMsg
    eval msg
  where
    eval :: BotMsg -> IrcPlugin ()
    eval (InternalCmd intCmd) = do
        case intCmdCmd intCmd of
            "RUN" -> let stuff = words $ intCmdMsg intCmd
                     in case head stuff of
                            "quote" -> lift $ trace $ "Quote module has been invoked for: " ++ (show intCmd)
                            _       -> lift $ trace $ show intCmd -- TODO : help message
            _     -> lift $ trace $ show intCmd
    eval (InputMsg msg) = return ()
    eval _ = return ()

