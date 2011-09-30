{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
-- | This module is an IRC plugin that manages quotes for posterity and legend
module Hsbot.Plugin.Quote
    ( quote
    , theQuote
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import qualified Data.Map as M
import Data.Maybe
import Data.SafeCopy
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import qualified Network.IRC as IRC
import System.Environment.XDG.BaseDir
import System.Random

import Hsbot.Message
import Hsbot.Types

-- | A quote element
data QuoteElt = QuoteElt
    { eltQuotee :: IRC.UserName
    , eltQuote  :: String
    } deriving (Show, Typeable)

type QuoteID = Int

-- | A quote object
data Quote = Quote
    { quoter    :: IRC.UserName
    , quoteFrom :: IRC.Channel
    , quotE     :: [QuoteElt]
    , quoteTime :: UTCTime
    , votes     :: Int
    , voters    :: M.Map IRC.UserName QuoteID
    } deriving (Show, Typeable)

emptyQuote :: Quote
emptyQuote = Quote { quoter = ""
                   , quoteFrom = ""
                   , quotE = []
                   , quoteTime = posixSecondsToUTCTime 0
                   , votes = 0
                   , voters = M.empty }

-- The Quote database
data QuoteDB = QuoteDB
    { nextQuoteId  :: QuoteID
    , quoteBotDB   :: M.Map QuoteID Quote
    , lockedQuotes :: M.Map QuoteID (IRC.UserName, UTCTime)
    , lastActive   :: M.Map IRC.Channel QuoteID
    } deriving (Show, Typeable)

emptyQuoteDB :: QuoteDB
emptyQuoteDB = QuoteDB { nextQuoteId  = 0
                       , quoteBotDB   = M.empty
                       , lockedQuotes = M.empty
                       , lastActive   = M.empty }

$(deriveSafeCopy 0 'base ''QuoteElt)
$(deriveSafeCopy 0 'base ''Quote)
$(deriveSafeCopy 0 'base ''QuoteDB)

-- | Quote database transactions
getQuote :: QuoteID -> Query QuoteDB (Maybe Quote)
getQuote quoteId = fmap (M.lookup quoteId) (asks quoteBotDB)

getQuoteDB :: Query QuoteDB (M.Map QuoteID Quote)
getQuoteDB = asks quoteBotDB

-- TODO : a function for cleaning locks

isQuoteLockedFor :: QuoteID -> IRC.UserName -> UTCTime -> Query QuoteDB (Maybe Bool)
isQuoteLockedFor quoteId requestor now = do
    theQuote <- fmap (M.lookup quoteId) (asks quoteBotDB)
    case theQuote of
        Just _ -> do
            currentLock <- fmap (M.lookup quoteId) (asks lockedQuotes)
            case currentLock of
                Just (owner, lockStamp) ->
                  if owner == requestor
                    then return $ Just True
                    else return . Just $ (addUTCTime 300 lockStamp > now)     -- Is the entry older than 5 min?
                Nothing -> return $ Just True
        Nothing -> return Nothing

lockQuoteIdFor :: QuoteID -> IRC.UserName -> IRC.Channel -> UTCTime -> Update QuoteDB ()
lockQuoteIdFor quoteId requestor channel now = get >>= \db -> put db { lockedQuotes = M.insert quoteId (requestor, now) (lockedQuotes db)
                                                             , lastActive = M.insert channel quoteId (lastActive db) }

deleteQuote :: QuoteID -> IRC.Channel -> Update QuoteDB ()
deleteQuote quoteId channel = get >>= \db -> put db { quoteBotDB = M.delete quoteId (quoteBotDB db)
                                            , lockedQuotes = M.delete quoteId (lockedQuotes db)
                                            , lastActive = M.delete channel (lastActive db) }

setQuote :: QuoteID -> Quote -> Update QuoteDB ()
setQuote quoteId theQuote = get >>= \db -> put db { quoteBotDB = M.insert quoteId theQuote (quoteBotDB db) }

getLastActiveQuote :: IRC.Channel -> Query QuoteDB (Maybe QuoteID)
getLastActiveQuote channel = fmap (M.lookup channel) (asks lastActive)

setLastActiveQuote :: IRC.Channel -> QuoteID -> Update QuoteDB ()
setLastActiveQuote channel quoteID = get >>= \db -> put db { lastActive = M.insert channel quoteID (lastActive db) }
takeNextQuoteID :: IRC.UserName -> IRC.Channel -> UTCTime -> Update QuoteDB (QuoteID)
takeNextQuoteID requestor channel now = do
    db <- get
    let quoteId = nextQuoteId db
    put db { nextQuoteId = nextQuoteId db + 1
           , lockedQuotes = M.insert quoteId (requestor, now) (lockedQuotes db)
           , lastActive = M.insert channel quoteId (lastActive db) }
    return quoteId

$(makeAcidic ''QuoteDB [ 'getQuote, 'getQuoteDB, 'isQuoteLockedFor, 'lockQuoteIdFor, 'deleteQuote, 'setQuote
                       , 'getLastActiveQuote, 'setLastActiveQuote, 'takeNextQuoteID ])

-- | gets a random quote from the database
getRandomQuote :: AcidState QuoteDB -> IO (Maybe (Quote, QuoteID))
getRandomQuote quoteDB = do
    db <- query' quoteDB GetQuoteDB
    if M.size db > 0
      then getStdRandom (randomR (0, M.size db - 1)) >>= \rInt -> return $ Just (snd (M.elemAt rInt db), rInt)
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
    quoteDB <- liftIO $ openAcidStateFrom (baseDir ++ "/quoteDB/") emptyQuoteDB
    forever $ readMsg >>= eval quoteDB
  where
    eval :: AcidState QuoteDB -> Message -> Plugin (Env IO) ()
    eval quoteDB (IncomingMsg msg)
        | IRC.msg_command msg == "PRIVMSG" = do
            cmdArgs <- lift $ getCommand msg
            case cmdArgs of
                "quote":"append":quoteID:quotee:quoteTxt ->
                    case reads quoteID :: [(Int, String)] of
                        (qid,_):_ -> quoteAppend quoteDB msg qid quotee $ unwords quoteTxt
                        _ -> do
                            lastQid <- query' quoteDB (GetLastActiveQuote (getChannel msg))
                            case lastQid of
                                Just qid -> quoteAppend quoteDB msg qid quotee . unwords $ quoteID : quoteTxt
                                Nothing -> answerMsg msg $ getSender msg ++ " : Invalid quoteID."
                "quote":"delete":quoteID:eltID ->
                    case reads quoteID :: [(Int, String)] of
                        (qid,_):_ -> case eltID of
                            [] -> quoteDelete quoteDB msg qid
                            eltID':[] -> case reads eltID' :: [(Int, String)] of
                                (eltid,_):_ -> quoteDeleteElt quoteDB msg qid eltid
                                _ -> answerMsg msg $ getSender msg ++ ": Invalid elementID."
                            _ -> answerMsg msg $ getSender msg ++ ": Invalid elementID."
                        _ -> answerMsg msg $ getSender msg ++ " : Invalid quoteID."
                "quote":"help":"append":_ -> answerMsg msg $ "quote append [QUOTEID] QUOTEE QUOTE"
                                             ++ "If no QUOTEID is provided, tries to append to the last active quote."
                "quote":"help":"delete":_ -> do
                    answerMsg msg "quote delete QUOTEID [ELTID] :"
                    answerMsg msg $ "  If an ELTID is provided, deletes the ELTID's line (starting from zero) "
                                 ++ "in the quote QUOTEID. If not the whole quote is deleted."
                "quote":"help":"start":_ -> do
                    answerMsg msg "quote [start] QUOTEE [QUOTE] :"
                    answerMsg msg $ "  Begins a quote for QUOTEE. You must provide the keywork start if the "
                                 ++ "QUOTEE's nickname is a reserved word for this quote module. If no QUOTE is "
                                 ++ "provided this module lookup it's conversation history and records the "
                                 ++ "last sentence of QUOTEE."
                "quote":"help":"show":_ -> answerMsg msg "quote show { QUOTEID | [random] }"
                "quote":"help":"stat":_ -> do
                    answerMsg msg "quote stat"
                    answerMsg msg "  Compute statistics about the quote database : Most quoters, most quoted "
                "quote":"help":[] ->
                    answerMsg msg $ "Usage: quote { [start] QUOTEE [QUOTE] | append [QUOTEID] QUOTEE QUOTE | "
                                 ++ "delete QUOTEID [ELTID] | show { QUOTEID | random [MIN_SCORE] } | stat }"
                "quote":"help":_ -> answerMsg msg "Invalid help topic."
                "quote":"show":"random":[] -> showRandomQuote
                "quote":"show":quoteID:[] ->
                    case reads quoteID :: [(Int, String)] of
                        (qid,_):_ -> do
                            thisQuote <- query' quoteDB (GetQuote qid)
                            case thisQuote of
                                Just this -> quoteShow quoteDB msg qid this
                                Nothing -> answerMsg msg $ (getSender msg) ++ ": Invalid quoteID or empty database."
                        _ -> answerMsg msg $ getSender msg ++ " : Invalid quoteID."
                "quote":"show":[] -> showRandomQuote
                "quote":"start":quotee:[phrase] -> quoteStart quoteDB msg quotee phrase
                "quote":quotee:[phrase] -> quoteStart quoteDB msg quotee phrase
                "quote":_ -> answerMsg msg "Invalid quote command."
                "vote":"help":"quick":_ -> do
                    answerMsg msg "vote [quick] [QUOTEID] { +1 | -1 | ++ | -- }"
                    answerMsg msg $ "  Vote for a quote. You can also vote for the last active quote on this chan "
                                 ++ "by typing something that begins by +1, -1 or ++ or --."
                "vote":"help":[] -> answerMsg msg "Usage: vote { [quick] [QUOTEID] { +1 | -1 } | show [QUOTEID] | stat }"
                "vote":"help":_ -> answerMsg msg "Invalid help topic."
                "vote":_ -> answerMsg msg "Invalid vote command."
                _ -> return ()
        | otherwise = return ()
      where
        showRandomQuote :: Plugin (Env IO) ()
        showRandomQuote = do
            rquote <- liftIO (getRandomQuote quoteDB)
            case rquote of
                Just (that, qid) -> quoteShow quoteDB msg qid that
                Nothing -> answerMsg msg $ (getSender msg) ++ ": the quote database is empty."
    eval _ _ = return ()

quoteAppend :: AcidState QuoteDB -> IRC.Message -> QuoteID -> IRC.UserName -> String -> Plugin (Env IO) ()
quoteAppend quoteDB msg quoteID quotee text = do
    now <- liftIO getCurrentTime
    activeLock <- query' quoteDB (IsQuoteLockedFor quoteID sender now)
    case activeLock of
        Just True -> do
            _ <- update' quoteDB (LockQuoteIdFor quoteID sender channel now)
            mQuote <- query' quoteDB (GetQuote quoteID)
            let newQuote = fromMaybe emptyQuote mQuote
                newQuote' = newQuote { quotE = quotE newQuote ++ [ QuoteElt { eltQuotee = quotee, eltQuote = text } ] }
            _ <- update' quoteDB (SetQuote quoteID newQuote')
            answerMsg msg $ sender ++ ": Appended to quote " ++ show quoteID ++ "."
        Just False -> answerMsg msg $ sender ++ ": Someone else is editing this quote right now."
        Nothing -> answerMsg msg $ sender ++ ":quoteId not found."
  where
    sender = getSender msg
    channel = getChannel msg

quoteDelete :: AcidState QuoteDB -> IRC.Message -> QuoteID -> Plugin (Env IO) ()
quoteDelete quoteDB msg quoteID = do
    now <- liftIO getCurrentTime
    activeLock <- query' quoteDB (IsQuoteLockedFor quoteID sender now)
    case activeLock of
        Just True -> do
            _ <- update' quoteDB (DeleteQuote quoteID channel)
            answerMsg msg $ sender ++ ": quote " ++ show quoteID ++ "."
        Just False -> answerMsg msg $ sender ++ ": Someone else is editing this quote right now."
        Nothing -> answerMsg msg $ sender ++ ":quoteId not found."
  where
    sender = getSender msg
    channel = getChannel msg

quoteDeleteElt :: AcidState QuoteDB -> IRC.Message -> QuoteID -> Int -> Plugin (Env IO) ()
quoteDeleteElt quoteDB msg quoteID eltID = do
    now <- liftIO getCurrentTime
    activeLock <- query' quoteDB (IsQuoteLockedFor quoteID sender now)
    case activeLock of
        Just True -> do
            _ <- update' quoteDB (LockQuoteIdFor quoteID sender channel now)
            mQuote <- query' quoteDB (GetQuote quoteID)
            let newQuote = fromMaybe emptyQuote mQuote
                newQuote' = newQuote { quotE = getRidOfEltFrom (quotE newQuote) }
            _ <- update' quoteDB (SetQuote quoteID newQuote')
            answerMsg msg $ sender ++ ": Appended to quote " ++ show quoteID ++ "."
        Just False -> answerMsg msg $ sender ++ ": Someone else is editing this quote right now."
        Nothing -> answerMsg msg $ sender ++ ": quoteId not found."
  where
    sender = getSender msg
    channel = getChannel msg
    getRidOfEltFrom :: [QuoteElt] -> [QuoteElt]
    getRidOfEltFrom elts
      | eltID <= 0 = elts
      | eltID >= length elts = elts
      | otherwise = let (l, r) = splitAt eltID elts
                    in l ++ tail r

quoteShow :: AcidState QuoteDB -> IRC.Message -> QuoteID -> Quote -> Plugin (Env IO) ()
quoteShow quoteDB msg quoteID thatQuote = do
    mapM_ (answerMsg msg) $ formatQuote
    update' quoteDB (SetLastActiveQuote channel quoteID)
  where
    channel = getChannel msg
    formatQuote :: [String]
    formatQuote = ("+-- [" ++ show quoteID ++ "] --- Reported by " ++ quoter thatQuote ++ " on " ++ quoteFrom thatQuote)
               : map formatElt (quotE thatQuote)
               ++ [ "+-- Added on " ++ show (quoteTime thatQuote) ++ " --- Score : " ++ show (votes thatQuote) ]
    formatElt :: QuoteElt -> String
    formatElt this = "| " ++ eltQuotee this ++ ": " ++ eltQuote this

quoteStart :: AcidState QuoteDB -> IRC.Message -> IRC.UserName -> String -> Plugin (Env IO) ()
quoteStart quoteDB msg quotee phrase =
    case phrase of
      [] -> answerMsg msg "TODO: implement history lookup"
      that -> quoteThat that
  where
    sender = getSender msg
    channel = getChannel msg
    quoteThat :: String -> Plugin (Env IO) ()
    quoteThat thatQuote = do
        now <- liftIO getCurrentTime
        quoteID <- update' quoteDB (TakeNextQuoteID sender channel now)
        let newQuote = emptyQuote { quoter = sender
                                  , quoteFrom = channel
                                  , quotE = [ QuoteElt { eltQuotee = quotee, eltQuote = thatQuote } ]
                                  , quoteTime = now }
        _ <- update' quoteDB (SetQuote quoteID newQuote)
        answerMsg msg $ sender ++ ": new quote added with ID " ++ show quoteID

