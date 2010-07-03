module Hsbot.Irc.Plugin.Quote
    ( ircBotPluginQuote
    ) where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory
import IO hiding (catch)
import Prelude hiding (catch)
import System.FilePath
import System.Posix.Files
import System.Random(randomRIO)

import Hsbot.Irc.Message
import Hsbot.Irc.Plugin.Utils

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
type QuoteBot a = StateT QuoteBotState (StateT IrcPluginState IO) a

-- | The plugin's main entry point
ircBotPluginQuote :: Chan IrcBotMsg -> Chan IrcBotMsg -> IO ()
ircBotPluginQuote myChan masterChan = do
    -- First of all we restore the database
    dir <- getAppUserDataDirectory "hsbot"
    let dbfile = dir </> "quotedb.txt"
    dbfileExists <- fileExist dbfile
    if not dbfileExists
      then
        let quoteBot = QuoteBotState 0 M.empty M.empty
        in TIO.writeFile dbfile (T.pack $ show quoteBot)
      else
        return ()
    txtQuoteBot <- TIO.readFile $ dbfile
    let quoteBot = read (T.unpack txtQuoteBot) :: QuoteBotState
    -- The plugin main loop
    let plugin = IrcPluginState { ircPluginName       = "Quote"
                                , ircPluginChan       = myChan
                                , ircPluginMasterChan = masterChan }
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
    eval :: IrcBotMsg -> IrcPlugin (QuoteBotState)
    eval (IntIrcCmd intCmd)
      | ircCmdCmd intCmd == "RUN" = do
          quoteBot' <- execStateT (runCommand intCmd) quoteBot
          return quoteBot'
      | otherwise = return quoteBot
    eval (InIrcMsg _) = return (quoteBot)
    eval (OutIrcMsg _) = return (quoteBot)

-- | run a command we received
runCommand :: IrcCmd -> QuoteBot ()
runCommand intCmd
  | theCommand == "quote" = runQuoteCommand
  | otherwise = return ()
  where
    -- | the message is a quote command
    runQuoteCommand :: QuoteBot ()
      | length args == 0 = do
          quoteDB <- gets quoteBotDB
          x <- liftIO $ randomRIO (0, (length $ M.keys quoteDB) - 1)
          if (length $ M.keys quoteDB) > 0
            then
              mapM_ (lift . answerMsg request) (formatQuote (M.keys quoteDB !! x) (M.elems quoteDB !! x))
            else
              lift $ answerMsg request "The quote database is empty."
      | otherwise = do
          dispatchQuoteCmd $ head args
    -- | quote command dispatcher
    dispatchQuoteCmd :: String -> QuoteBot ()
    dispatchQuoteCmd cmd
      | cmd == "start"  = do
          quoteBot <- get
          now <- liftIO $ getCurrentTime
          let sender   = takeWhile (/= '!') $ fromMaybe "ARGH" (ircMsgPrefix request)
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
    params  = words . ircCmdMsg $ intCmd
    theCommand = head params
    args    = tail params
    stuff   = tail args
    request = ircCmdBotMsg intCmd

-- | The function that sync the quoteDB on disk
syncQuoteBot :: QuoteBot ()
syncQuoteBot = do
    dir <- liftIO $ getAppUserDataDirectory "hsbot"
    let dbfile = dir </> "quotedb.txt"
    file' <- liftIO $ openFile dbfile WriteMode
    quoteBot <- get
    liftIO . hPutStr file' $ show quoteBot
    liftIO $ hClose file'

formatQuote :: Integer -> Quote -> [String]
formatQuote quoteId theQuote =
    ("+---| " ++ (show quoteId) ++ " |-- Reported by " ++ (quoter theQuote) ++ " on " ++ (show $ quoteTime theQuote)) :
    foldl (\acc x -> formatQuoteElt x : acc) ["`------------------------------------------"] (quote theQuote)
  where
    formatQuoteElt :: QuoteElt -> String
    formatQuoteElt quoteElt = "| <" ++ (eltQuoter quoteElt) ++ "> " ++ (eltQuote quoteElt)

