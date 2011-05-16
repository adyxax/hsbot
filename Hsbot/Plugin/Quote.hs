{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
-- | This module is an IRC plugin that manages quotes for posterity and legend
module Hsbot.Plugin.Quote
    () where

import qualified Data.Map as M
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

