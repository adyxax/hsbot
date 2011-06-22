{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
-- | This module is an IRC plugin that manages quotes for posterity and legend
module Hsbot.Plugin.Quote
--    ( quote
--    , theQuote
    () where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import qualified Data.Map as M
import qualified Network.IRC as IRC
import System.Random
import Data.SafeCopy
import Data.Typeable
import System.Time

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

-- | A quote element
data QuoteElt = QuoteElt
    { eltQuotee :: IRC.UserName
    , eltQuote  :: String
    } deriving (Show, Typeable)

type QuoteId = Int

-- | A quote object
data Quote = Quote
    { quoter    :: IRC.UserName
    , quote     :: [QuoteElt]
    , quoteTime :: ClockTime
    , votes     :: Int
    , voters    :: M.Map IRC.UserName QuoteID
    } deriving (Show, Typeable)

emptyQuote :: Quote
emptyQuote = Quote { quoter = ""
                   , quote = []
                   , quoteTime = TOD 0 0
                   , votes = 0
                   , voters = M.empty }

-- The Quote database
data QuoteDB = QuoteDB
    { nextQuoteId  :: QuoteID
    , quoteBotDB   :: M.Map QuoteID Quote
    , lockedQuotes :: M.Map QuoteID (IRC.UserName, ClockTime)
    , lastActive   :: M.Map IRC.Channel QuoteID
    } deriving (Show, Typeable)

emptyQuoteDB :: QuoteDB
emptyQuoteDB = QuoteDB { nextQuoteId  = 0
                       , quoteBotDB   = M.empty
                       , lockedQuotes = M.empty
                       , lastActive   = Nothing }

$(deriveSafeCopy 0 'base ''QuoteElt)
$(deriveSafeCopy 0 'base ''Quote)
$(deriveSafeCopy 0 'base ''QuoteDB)

-- | Quote database transactions
getQuote :: QuoteID -> Query QuoteDB (Maybe Quote)
getQuote quoteId = asks quoteBotDB >>= return . M.lookup quoteId

getQuoteDB :: Query QuoteDB (M.Map QuoteID Quote)
getQuoteDB = asks quoteBotDB

-- TODO : a function for cleaning locks

isQuoteLockedFor :: QuoteID -> IRC.UserName -> ClockTime -> Query QuoteDB (Either String Bool)
isQuoteLockedFor quoteId requestor now = do
    theQuote <- asks quoteBotDB >>= return . M.lookup quoteId
    case theQuote of
        Just quote -> do
            currentLock <- asks lockedQuotes >>= return . M.lookup quoteId
            case currentLock of
                Just (owner, lockStamp) ->
                  if owner == requestor
                    then return $ Right True
                    else return . Right $ (addToClockTime (TimeDiff 0 0 0 0 5 0 0) lockStamp > now)     -- Is the entry older than 5 min?
                Nothing -> return $ Right True
        Nothing -> return $ Left "QuoteId not found"

lockQuoteIdFor :: QuoteID -> IRC.UserName -> ClockTime -> Update QuoteDB ()
lockQuoteIdFor quoteId requestor now = get >>= \db -> put db { lockedQuotes = M.insert quoteId (requestor, now) (lockedQuotes db) }

$(makeAcidic ''QuoteDB ['getQuote, 'getQuoteDB, 'isQuoteLockedFor, 'lockQuoteIdFor])

-- | gets a random quote from the database
getRandomQuote :: AcidState QuoteDB -> IO (Maybe Quote)
getRandomQuote quoteDB = do
    db <- query' quoteDB GetQuoteDB
    if M.size db > 0
      then getStdRandom (randomR (0, M.size db - 1)) >>= \rInt -> return . Just . snd $ M.elemAt rInt db
      else return Nothing

-- | The duck plugin identity
quote :: PluginId
quote = PluginId
    { pluginName = "quote"
    , pluginEp   = theQuote }

-- | An IRC plugin that generates and kills ducks
theQuote :: Plugin (Env IO) ()  -- TODO : an argument for the history size
theQuote = do
    baseDir <- liftIO $ System.Environment.XDG.BaseDir.getUserDataDir "hsbot"
    statDB <- liftIO $ openAcidStateFrom (baseDir ++ "/quoteDB/") emptyQuoteDB
    forever $ readMsg >>= eval quoteDB
  where
    eval :: AcidState QuoteDB -> Message -> Plugin (Env IO) ()
    eval quoteDB (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = do
            cmdArgs <- lift $ getCommand msg
            case cmdArgs of
                "quote":"help":"append":_ -> answerMsg "quote append QUOTEID QUOTEE QUOTE"
                "quote":"help":"delete":_ -> do
                    answerMsg "quote delete QUOTEID [ELTID] :"
                    answerMsg $ concat [ "  If an ELTID is provided, deletes the ELTID's line (starting from zero) "
                                       , "in the quote QUOTEID. If not the whole quote is deleted." ]
                "quote":"help":"quick":_ -> do
                    answerMsg "quote [quick] QUOTEE [QUOTE] :"
                    answerMsg $ concat [ "  Begins a quote for QUOTEE. You must provide the keywork quick if the "
                                       , "QUOTEE's nickname is a reserved word for this quote module. If no QUOTE is "
                                       , "provided this module lookup it's conversation history and records the "
                                       , "last sentence of QUOTEE." ]
                "quote":"help":"show":_ -> answerMsg "quote show { QUOTEID | random [MIN_SCORE] }"
                "quote":"help":"stat":_ -> do
                    answerMsg "quote stat"
                    answerMsg "  Compute statistics about the quote database : Most quoters, most quoted "
                "quote":"help":[] -> do
                    answerMsg $ concat [ "Usage: quote { [quick] QUOTEE [QUOTE] | append QUOTEID QUOTEE QUOTE | "
                                       , "delete QUOTEID [ELTID] | show { QUOTEID | random [MIN_SCORE] } | stat }" ]
                "quote":"help":_ -> answerMsg "Invalid help topic."
                "quote":_ -> answerMsg msg "Invalid quote command."
                "vote":"help":"quick":_ -> do
                    answerMsg "vote [quick] [QUOTEID] { +1 | -1 | ++ | -- }"
                    answerMsg $ concat [ "  Vote for a quote. You can also vote for the last active quote on this chan "
                                       , "by typing something that begins by +1, -1 or ++ or --." ]
                "vote":"help":[] -> answerMsg $ concat [ "Usage: vote { [quick] [QUOTEID] { +1 | -1 } | show [QUOTEID] | stat }"
                "vote":"help":_ -> answerMsg "Invalid help topic."
                "vote":_ -> answerMsg msg "Invalid vote command."
                _ -> return ()
        | otherwise = return ()
    eval _ _ _ _ = return ()

