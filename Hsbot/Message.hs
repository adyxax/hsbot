module Hsbot.Message
    ( processInternalMessage
    , processRebootMessage
    , processExitMessage
    ) where

import Control.Monad.State
import qualified Data.Map as M

import Hsbot.PluginUtils
import Hsbot.Types

-- | Processes an internal message
processInternalMessage :: Msg -> Bot (BotStatus)
processInternalMessage msg
  | msgTo msg == "CORE" = processCoreMessage msg
  | otherwise = do
      plugins <- gets botPlugins
      case M.lookup (msgTo msg) plugins of
          Just (plugin, _, _) -> sendToPlugin (IntMsg msg) plugin
          Nothing          -> return ()
      return BotContinue

processCoreMessage :: Msg -> Bot (BotStatus)
processCoreMessage msg = do
    case msgType msg of
        "REBOOT" -> return BotReboot
        _        -> return BotContinue

processRebootMessage :: RebootMsg -> Bot (BotStatus)
processRebootMessage _ = return BotReboot   -- TODO : check who is sending that to us

processExitMessage :: ExitMsg -> Bot (BotStatus)
processExitMessage _ = return BotExit   -- TODO : check who is sending that to us

