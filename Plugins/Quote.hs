module Plugins.Quote
    ( mainQuote
    ) where

import Control.Concurrent.Chan
import Control.Monad.State
import System.Time (ClockTime)

import Hsbot.Core
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

-- | The main function of the Quote module
mainQuote :: Chan BotMsg -> Chan BotMsg -> IO ()
mainQuote serverChan chan = do
    writeChan serverChan $ InternalCmd $ IntCmd "REGISTER COMMAND quote Quote" emptyMsg
    loop
    where
        loop = do
            input <- readChan chan
            eval input
            loop
        eval :: BotMsg -> IO ()
        eval (InternalCmd intCmd) = do
            let command' = words $ internalCommand intCmd
            case command' !! 0 of
                "runCommand" -> case (command' !! 1) of
                                    "quote" -> writeChan serverChan $ OutputMsg $ internalCommandMsg intCmd
                                    _       -> trace $ show command' -- TODO : help message
                _            -> trace $ show command'
        eval (InputMsg msg) = return ()
        eval _ = return ()

