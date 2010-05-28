module Hsbot.Message
    ( processInternalMessage
    ) where

import Control.Monad.State
import qualified Data.Map as M

import Hsbot.PluginUtils
import Hsbot.Types

-- | Processes an internal message
processInternalMessage :: BotMsg -> Bot (BotStatus)
processInternalMessage (IntMsg msg)
  | msgTo msg == "CORE" = processCoreMessage msg
  | otherwise = do
      plugins <- gets botPlugins
      case M.lookup (msgTo msg) plugins of
          Just (plugin, _) -> sendToPlugin (IntMsg msg) plugin
          Nothing          -> return ()
      return BotContinue
processInternalMessage _ = return BotContinue

processCoreMessage :: Msg -> Bot (BotStatus)
processCoreMessage msg = do
    case msgType msg of
        "REBOOT" -> return BotReboot
        _        -> return BotContinue

