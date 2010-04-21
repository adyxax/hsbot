module Hsbot.IRCParser
    ( ParseError
    , parseIrcMsg
    , serializeIrcMsg
    ) where

import Control.Monad.Identity
import Text.Parsec

import Hsbot.Types

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
    _ <- char ':'
    pfx <- many1 (noneOf " ")
    _ <- space
    return pfx

pCommand :: ParsecT String u Identity [Char]
pCommand = count 3 digit <|> many1 upper

pLongParam :: ParsecT String u Identity [Char]
pLongParam = char ':' >> (many1 (noneOf "\r"))

pShortParam :: ParsecT String u Identity [Char]
pShortParam = many1 (noneOf " \r")

-- |Serialize an IRC message to a string.
serializeIrcMsg :: IrcMsg -> String
serializeIrcMsg (IrcMsg pfx cmd params) = pfxStr ++ cmd ++ paramStr
    where pfxStr = case pfx of
                        Nothing  -> ""
                        Just pfx' -> ":" ++ pfx' ++ " "
          paramStr = concat (map paramToStr (init params)
                             ++ [lastParamToStr (last params)])
          paramToStr p = " " ++ p
          lastParamToStr p = " :" ++ p

