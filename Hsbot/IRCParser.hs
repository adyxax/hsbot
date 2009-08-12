module Hsbot.IRCParser
    ( IrcMsg (..)
    , ircParser
    ) where

--import Text.Parsec
import Text.ParserCombinators.Parsec

-- |An IRC message.
data IrcMsg = IrcMsg (Maybe String) String [String] -- (Maybe first statement) cmd [chan, params/sentence]
    deriving (Show)

--ircParser :: String -> IrcInput
ircParser :: String -> Either ParseError IrcMsg
ircParser str = parse pMsg "" str

pMsg = do
    pfx <- optionMaybe pPrefix
    cmd <- pCommand
    params <- many (char ' ' >> (pLongParam <|> pShortParam))
    char '\r'
    eof
    return $ IrcMsg pfx cmd params

pPrefix = do
    char ':'
    pfx <- many1 (noneOf " ")
    space
    return pfx

pCommand = count 3 digit <|> many1 upper

pLongParam = char ':' >> (many1 (noneOf "\r"))

pShortParam = many1 (noneOf " \r")

