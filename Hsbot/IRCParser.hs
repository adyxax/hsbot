module Hsbot.IRCParser
    ( ParseError
    , parseIrcMsg
    ) where

import Control.Monad.Identity
-- import Data.List
import Text.Parsec

import Hsbot.Core

-- | Parses an IrcInput
parseIrcMsg :: String -> Either ParseError IrcMsg
parseIrcMsg line = parse pMsg "" line

pMsg :: ParsecT String u Identity IrcMsg
pMsg = do
    pfx <- optionMaybe pPrefix
    cmd <- pCommand
    params <- many (char ' ' >> (pLongParam <|> pShortParam))
    --char '\r'
    eof
    return $ IrcMsg pfx cmd params

pPrefix :: ParsecT String u Identity [Char]
pPrefix = do
    char ':'
    pfx <- many1 (noneOf " ")
    space
    return pfx

pCommand :: ParsecT String u Identity [Char]
pCommand = count 3 digit <|> many1 upper

pLongParam :: ParsecT String u Identity [Char]
pLongParam = char ':' >> (many1 (noneOf "\r"))

pShortParam :: ParsecT String u Identity [Char]
pShortParam = many1 (noneOf " \r")

