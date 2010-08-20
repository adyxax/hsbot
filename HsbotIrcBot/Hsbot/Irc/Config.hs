module Hsbot.Irc.Config
    ( IrcConfig(..)
    , ircDefaultConfig
    , getIrcConfig
    ) where

import Control.Monad.Error
import Data.Char (isDigit)
import qualified Data.ConfigFile as C
import Data.Either.Utils
import Network
import System.Exit
import System.Posix.Files

-- | Configuration data type
data IrcConfig = IrcConfig
    { ircConfigAddress       :: String   -- the server's address
    , ircConfigPort          :: PortID   -- the server's port
    , ircConfigChannels      :: [String] -- the Channels to join on start
    , ircConfigNickname      :: String   -- the hsbot's nickname
    , ircConfigPassword      :: String   -- the hsbot's password, optional
    , ircConfigRealname      :: String   -- the hsbot's real name, optional
    , ircConfigCommandPrefix :: Char     -- the prefix the ircbot will recognize as commands
    , ircConfigPlugins       :: [String] -- the ircPlugins to load
    }

instance Show IrcConfig where
    show (IrcConfig address port channels nickname password realname commandPrefix plugins) = unlines $
        concat [ "Address: ", address ] :
        concat [ "Port: ", case port of
                            PortNumber num -> show num
                            Service s      -> show s
                            UnixSocket u   -> show u ] :
        concat [ "Channels: ", show channels ] :
        concat [ "Nickname: ", nickname ] :
        concat [ "Password: ", password ] :
        concat [ "Realname: ", realname ] :
        concat [ "CommandPrefix: ", show commandPrefix ] :
        [ "Plugins: ", show plugins ]

-- | User configuration
ircDefaultConfig :: IrcConfig
ircDefaultConfig = IrcConfig
    { ircConfigAddress       = "localhost"
    , ircConfigPort          = PortNumber 6667
    , ircConfigChannels      = ["#hsbot"]
    , ircConfigNickname      = "hsbot"
    , ircConfigPassword      = ""
    , ircConfigRealname      = "The One True bot, with it's haskell soul."
    , ircConfigCommandPrefix = '@'
    , ircConfigPlugins       = ["Ping", "Core"]
    }

-- | config file retrieving
getIrcConfig :: Maybe String -> IO (IrcConfig)
getIrcConfig maybePath =
    case maybePath of
        Just path -> do
            doesFileExists <- fileExist path
            case doesFileExists of
                True -> do
                    fileStatus <- getFileStatus path
                    case isRegularFile $ fileStatus of
                        True -> getConfigFromFile path
                        False -> do
                            putStrLn "Invalid configuration file path."
                            exitWith $ ExitFailure 1
                False -> do
                    putStrLn "The specified configuration file does not exists."
                    exitWith $ ExitFailure 1
        Nothing -> return ircDefaultConfig  -- TODO : try defaults like $HOME/.hsbotrc, /etc/hsbotrc or /usr/local/etc/hsbotrc

-- | Get configuration from config file
getConfigFromFile :: FilePath -> IO IrcConfig
getConfigFromFile fname = readfile C.emptyCP fname >>= extractConfig . forceEither

-- | A version of readfile that treats the file as UTF-8
readfile :: MonadError C.CPError m => C.ConfigParser -> FilePath -> IO (m C.ConfigParser)
readfile cp path' = do
    contents <- readFile path'
    return $ C.readstring cp contents

-- | config file processing
extractConfig :: C.ConfigParser -> IO IrcConfig
extractConfig cp = do
    config' <- runErrorT $ do
        cfAddress <- getit "address"
        cfPort <- getit "port"
        cfChannels <- getit "channels"
        cfNickname <- getit "nickname"
        cfPassword <- getit "password"
        cfRealname <- getit "realname"
        cfCommandPrefix <- getit "commandprefix"
        cfPlugins <- getit "plugins"
        return $! IrcConfig {
                      ircConfigAddress       = cfAddress
                    , ircConfigPort          = PortNumber . fromIntegral $ readInteger "port" cfPort
                    , ircConfigChannels      = splitCommaList cfChannels
                    , ircConfigNickname      = cfNickname
                    , ircConfigPassword      = cfPassword
                    , ircConfigRealname      = cfRealname
                    , ircConfigCommandPrefix = readChar "commandprefix" cfCommandPrefix
                    , ircConfigPlugins       = splitCommaList cfPlugins }
    case config' of
         Left (C.ParseError e, e') -> error $ "Parse error: " ++ e ++ "\n" ++ e'
         Left e                  -> error (show e)
         Right c                 -> return c
  where
    getit = C.get cp "IRC"

readChar :: String -> String -> Char
readChar _ x | length x == 1 = head x
readChar opt _ = error $ opt ++ " must be one character long."

readInteger :: String -> String -> Int
readInteger _   x | all isDigit x = read x ::Int
readInteger opt _ = error $ opt ++ " must be an integer."

-- readNumber :: (Num a, Read a) => String -> String -> a
-- readNumber _   x | all isDigit x = read x
-- readNumber opt _ = error $ opt ++ " must be a number."

splitCommaList :: String -> [String]
splitCommaList l =
    let (first, rest) = break (== ',') l
        first' = lrStrip first
    in case rest of
            []     -> if null first' then [] else [first']
            (_:rs) -> first' : splitCommaList rs

lrStrip :: String -> String
lrStrip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
    where isWhitespace = (`elem` " \t\n")

