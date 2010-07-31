module Hsbot.Irc.Config
    ( IrcConfig(..)
    , ircDefaultConfig
    , getIrcConfig
    ) where

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
    , ircConfigPlugins       = ["Ping"]
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
                        True -> compileIrcConfig ircDefaultConfig path
                        False -> do
                            putStrLn "Invalid configuration file path."
                            exitWith $ ExitFailure 1
                False -> do
                    putStrLn "The specified configuration file does not exists."
                    exitWith $ ExitFailure 1
        Nothing -> return ircDefaultConfig  -- TODO : try defaults like $HOME/.hsbotrc, /etc/hsbotrc or /usr/local/etc/hsbotrc

-- | config file parsing
compileIrcConfig :: IrcConfig -> String -> IO (IrcConfig)
compileIrcConfig ircConfig path = do
    val <- C.readfile C.emptyCP path
    let cp = forceEither val
    let address = case C.get cp "IRC" "address" of
         Right this -> this
         Left _ -> ircConfigAddress ircConfig
    let port = case C.get cp "IRC" "port" of
         Right this -> PortNumber $ fromIntegral (read this :: Int) -- TODO error handling
         Left _ -> ircConfigPort ircConfig
    let channels = case C.get cp "IRC" "channels" of
         Right this -> map (lstrip ' ') (split this ',')
         Left _ -> ircConfigChannels ircConfig
    let nickname = case C.get cp "IRC" "nickname" of
         Right this -> this
         Left _ -> ircConfigNickname ircConfig
    let password = case C.get cp "IRC" "password" of
         Right this -> this
         Left _ -> ircConfigPassword ircConfig
    let realname = case C.get cp "IRC" "realname" of
         Right this -> this
         Left _ -> ircConfigRealname ircConfig
    let commandPrefix = case C.get cp "IRC" "commandPrefix" of
         Right this -> head this -- TODO error handling
         Left _ -> ircConfigCommandPrefix ircConfig
    let plugins = case C.get cp "IRC" "plugins" of
         Right this -> map (lstrip ' ') (split this ',')
         Left _ -> ircConfigPlugins ircConfig
    return ircConfig { ircConfigAddress       = address
                     , ircConfigPort          = port
                     , ircConfigChannels      = channels
                     , ircConfigNickname      = nickname
                     , ircConfigPassword      = password
                     , ircConfigRealname      = realname
                     , ircConfigCommandPrefix = commandPrefix
                     , ircConfigPlugins       = plugins }
  where
    split :: String -> Char -> [String]
    split [] _ = [""]
    split (c:cs) delim
       | c == delim = "" : rest
       | otherwise = (c : head rest) : tail rest
      where rest = split cs delim
    lstrip :: Char -> String -> String
    lstrip x (c:cs) = if (x == c) then (lstrip x cs) else c:(lstrip x cs)
    lstrip _ [] = []

