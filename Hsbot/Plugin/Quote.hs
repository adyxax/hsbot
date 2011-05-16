{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
-- | This module is an IRC plugin that manages quotes for posterity and legend
module Hsbot.Plugin.Quote
    () where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import qualified Data.Map as M
import System.Random
import Data.SafeCopy
import Data.Typeable
import System.Time

import Hsbot.Message
import Hsbot.Types
import Hsbot.Utils

-- | A quote element
data QuoteElt = QuoteElt
    { eltQuotee :: String
    , eltQuote  :: String
    } deriving (Show, Typeable)

-- | A quote object
data Quote = Quote
    { quoter    :: String
    , quote     :: [QuoteElt]
    , quoteTime :: ClockTime
    , votes     :: Int
    , voters    :: M.Map String Int
    } deriving (Show, Typeable)

emptyQuote :: Quote
emptyQuote = Quote { quoter = ""
                   , quote = []
                   , quoteTime = TOD 0 0
                   , votes = 0
                   , voters = M.empty }

-- The Quote database
data QuoteDB = QuoteDB
    { nextQuoteId  :: Int
    , quoteBotDB   :: M.Map Int Quote
    , lockedQuotes :: M.Map Int (String, ClockTime)
    } deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''QuoteElt)
$(deriveSafeCopy 0 'base ''Quote)
$(deriveSafeCopy 0 'base ''QuoteDB)

-- | Quote database transactions
getQuote :: Int -> Query QuoteDB (Maybe Quote)
getQuote quoteId = asks quoteBotDB >>= return . M.lookup quoteId

getQuoteDB :: Query QuoteDB (M.Map Int Quote)
getQuoteDB = asks quoteBotDB

isQuoteLockedFor :: Int -> String -> ClockTime -> Query QuoteDB (Either String Bool)
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

lockQuoteIdFor :: Int -> String -> ClockTime -> Update QuoteDB ()
lockQuoteIdFor quoteId requestor now = get >>= \db -> put db { lockedQuotes = M.insert quoteId (requestor, now) (lockedQuotes db) }

$(makeAcidic ''QuoteDB ['getQuote, 'getQuoteDB, 'isQuoteLockedFor, 'lockQuoteIdFor])

-- | gets a random quote from the database
getRandomQuote :: AcidState QuoteDB -> IO (Maybe Quote)
getRandomQuote quoteDB = do
    db <- query' quoteDB GetQuoteDB
    if M.size db > 0
      then getStdRandom (randomR (0, M.size db - 1)) >>= \rInt -> return . Just . snd $ M.elemAt rInt db
      else return Nothing

