module Hsbot.Irc.Message
    ( IrcBotMsg (..)
    , IrcCmd (..)
    , IrcMsg (..)
    , emptyIrcMsg
    , parseIrcMsg
    , serializeIrcMsg
    ) where

import Control.Monad.Identity
import Text.ParserCombinators.Parsec

-- | An IRC message
data IrcMsg = IrcMsg
    { ircMsgPrefix     :: Maybe String -- the message prefix
    , ircMsgCommand    :: String       -- the message command
    , ircMsgParameters :: [String]     -- the message parameters
    } deriving (Show)

emptyIrcMsg :: IrcMsg
emptyIrcMsg = IrcMsg Nothing "" []

-- | An internal command
data IrcCmd = IrcCmd
    { ircCmdCmd    :: String -- the internal command
    , ircCmdFrom   :: String -- who issues it
    , ircCmdTo     :: String -- who it is destinated to
    , ircCmdMsg    :: String -- the message to be transfered
    , ircCmdBotMsg :: IrcMsg -- An IrcMsg attached to the command
    } deriving (Show)

data IrcBotMsg = InIrcMsg IrcMsg | OutIrcMsg IrcMsg | IntIrcCmd IrcCmd deriving (Show)

-- | Parses an IrcInput
parseIrcMsg :: String -> Either ParseError IrcMsg
parseIrcMsg line = parse pMsg "" line

--pMsg :: Parser String u Identity IrcMsg
pMsg = do
    pfx <- optionMaybe pPrefix
    cmd <- pCommand
    params <- many (char ' ' >> (pLongParam <|> pShortParam))
    _ <- char '\r'
    eof
    return $ IrcMsg pfx cmd params

--pPrefix :: Parser String u Identity [Char]
pPrefix = do
    _ <- char ':'
    pfx <- many1 (noneOf " ")
    _ <- space
    return pfx

--pCommand :: Parser String u Identity [Char]
pCommand = count 3 digit <|> many1 upper

--pLongParam :: Parser String u Identity [Char]
pLongParam = char ':' >> (many1 (noneOf "\r"))

--pShortParam :: Parser String u Identity [Char]
pShortParam = many1 (noneOf " \r")

-- | Serialize an IRC message to a string.
serializeIrcMsg :: IrcMsg -> String
serializeIrcMsg (IrcMsg pfx cmd params) = pfxStr ++ cmd ++ paramStr
    where pfxStr = case pfx of
                        Nothing  -> ""
                        Just pfx' -> ":" ++ pfx' ++ " "
          paramStr = concat (map paramToStr (init params)
                             ++ [lastParamToStr (last params)])
          paramToStr p = " " ++ p
          lastParamToStr p = " :" ++ p

