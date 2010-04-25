module Plugins.Quote
    ( mainQuote
    ) where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import IO hiding (catch)
import Prelude hiding (catch)
import System.Random(randomRIO)

import Hsbot.IRCPlugin
import Hsbot.Types
import Hsbot.Utils

-- | A quote element
data QuoteElt = QuoteElt
    { eltQuoter :: String
    , eltQuote  :: String
    } deriving (Read, Show)

-- | A quote object
data Quote = Quote
    { quoter    :: String
    , quote     :: [QuoteElt]
    , quoteTime :: UTCTime
    , votes     :: Int
    } deriving (Read, Show)

-- | A QuoteBot state
data QuoteBotState = QuoteBotState
    { nextQuoteId      :: Integer
    , quoteBotDB       :: M.Map Integer Quote
    , quotesInProgress :: M.Map Integer Quote
    } deriving (Read, Show)

-- | The QuoteBot monad
type QuoteBot a = StateT QuoteBotState (StateT PluginState IO) a

-- | The plugin's main entry point
mainQuote :: Chan BotMsg -> Chan BotMsg -> IO ()
mainQuote serverChan chan = do
    -- First of all we restore the database
    txtQuoteBot <- TIO.readFile $ "quotedb.txt"
    let quoteBot = read (T.unpack txtQuoteBot) :: QuoteBotState
    -- The plugin main loop
    let plugin = PluginState "Quote" serverChan chan
    evalStateT (mapM_ sendRegisterCommand ["quote"]) plugin
    _ <- (evalStateT (run quoteBot) plugin) `catch` (\(_ :: AsyncException) -> return quoteBot)
    evalStateT (mapM_ sendUnregisterCommand ["quote"]) plugin

-- | The IrcPlugin monad main function
run :: QuoteBotState -> IrcPlugin (QuoteBotState)
run quoteBot = do
    msg <- readMsg
    quoteBot' <- eval msg
    run quoteBot'
  where
    -- | evaluate what we just received
    eval :: BotMsg -> IrcPlugin (QuoteBotState)
    eval (InternalCmd intCmd)
      | intCmdCmd intCmd == "RUN" = do
          quoteBot' <- execStateT (runCommand intCmd) quoteBot
          return quoteBot'
      | otherwise = do
          lift . trace $ show intCmd
          return quoteBot
    eval (InputMsg _) = return (quoteBot)
    eval _ = return (quoteBot)

-- | run a command we received
runCommand :: IntCmd -> QuoteBot ()
runCommand intCmd
  | theCommand == "quote" = runQuoteCommand
  | otherwise = do
      lift . lift . trace $ show intCmd -- TODO : help message
  where
    -- | the message is a quote command
    runQuoteCommand :: QuoteBot ()
      | length args == 0 = do
          quoteDB <- gets quoteBotDB
          x <- liftIO $ randomRIO (0, (length $ M.keys quoteDB) - 1)
          mapM_ (lift . answerMsg request) (formatQuote (M.keys quoteDB !! x) (M.elems quoteDB !! x))
      | otherwise = do
          dispatchQuoteCmd $ head args
    -- | quote command dispatcher
    dispatchQuoteCmd :: String -> QuoteBot ()
    dispatchQuoteCmd cmd
      | cmd == "start"  = do
          quoteBot <- get
          now <- liftIO $ getCurrentTime
          let sender   = takeWhile (/= '!') $ fromMaybe "ARGH" (prefix request)
              newQuote = Quote sender [(quoteElt stuff)] now 0
              quoteId     = nextQuoteId quoteBot
              quotesInProgress' = M.insert quoteId  newQuote (quotesInProgress quoteBot)
          put $ quoteBot { nextQuoteId = quoteId + 1, quotesInProgress = quotesInProgress' }
          lift $ answerMsg request ("New quoteId : " ++ show quoteId)
          syncQuoteBot
      | cmd == "append" = do
          quoteBot <- get
          case reads (head stuff) of
              [(quoteId :: Integer,"")] -> do
                  case M.lookup quoteId (quotesInProgress quoteBot) of
                      Just theQuote -> do
                          let newQuote = theQuote { quote = (quoteElt $ tail stuff) : (quote theQuote) }
                              quotesInProgress'  = M.insert quoteId newQuote (quotesInProgress quoteBot)
                          put $ quoteBot { quotesInProgress = quotesInProgress' }
                          syncQuoteBot
                      Nothing       -> lift $ answerMsg request ("quoteId not found : " ++ (show quoteId))
              _ -> lift $ answerMsg request ("Invalid quoteId : " ++ (head stuff))
      | cmd == "commit" = do
          quoteBot <- get
          case reads (head stuff) of
              [(quoteId :: Integer,"")] -> do
                  case M.lookup quoteId (quotesInProgress quoteBot) of
                      Just theQuote -> do
                          let quoteBotDB'       = M.insert quoteId theQuote (quoteBotDB quoteBot)
                              quotesInProgress' = M.delete quoteId (quotesInProgress quoteBot)
                          put $ quoteBot { quoteBotDB = quoteBotDB', quotesInProgress = quotesInProgress' }
                          syncQuoteBot
                      Nothing       -> lift $ answerMsg request ("quoteId not found : " ++ (show quoteId))
              _ -> lift $ answerMsg request ("Invalid quoteId : " ++ (head stuff))
      -- | cmd == "abort"  = 
      | otherwise = lift $ answerMsg request ("Invalid command : " ++ cmd)
    -- | Gets the new QuoteElt
    quoteElt :: [String] -> QuoteElt
    quoteElt msg = do
        let budy   = head $ msg
            theQuote  = unwords . tail $ msg
        QuoteElt budy theQuote
    -- | utilities
    params  = words . intCmdMsg $ intCmd
    theCommand = head params
    args    = tail params
    stuff   = tail args
    request = intCmdBotMsg intCmd

-- | The function that sync the quoteDB on disk
syncQuoteBot :: QuoteBot ()
syncQuoteBot = do
    quoteBot <- get
    file' <- liftIO $ openFile "quotedb.txt" WriteMode
    liftIO . hPutStr file' $ show quoteBot
    liftIO $ hClose file'

formatQuote :: Integer -> Quote -> [String]
formatQuote quoteId theQuote =
    ("+---| " ++ (show quoteId) ++ " |-- Reported by " ++ (quoter theQuote) ++ " on " ++ (show $ quoteTime theQuote)) :
    foldl (\acc x -> formatQuoteElt x : acc) ["`------------------------------------------"] (quote theQuote)
  where
    formatQuoteElt :: QuoteElt -> String
    formatQuoteElt quoteElt = "| <" ++ (eltQuoter quoteElt) ++ "> " ++ (eltQuote quoteElt)

