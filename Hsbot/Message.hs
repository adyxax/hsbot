module Hsbot.Message
    ( processInternalMessage
    ) where

import Control.Monad.State
import qualified Data.Map as M

import Hsbot.PluginUtils
import Hsbot.Types

-- | Processes an internal message
processInternalMessage :: BotMsg -> Bot ()
processInternalMessage (IntMsg msg)
  | msgTo msg == "CORE" = processCoreMessage msg
  | otherwise = do
      plugins <- gets botPlugins
      case M.lookup (msgTo msg) plugins of
          Just (plugin, _) -> sendToPlugin (IntMsg msg) plugin
          Nothing          -> return ()
processInternalMessage _ = return ()

processCoreMessage :: Msg -> Bot ()
processCoreMessage msg = do
    case msgCmd msg of
        "UPDATE"     -> processUpdateCommand msg
        _            -> return ()

-- | Process an update command
processUpdateCommand :: Msg -> Bot ()
processUpdateCommand msg = do
    bot <- get
    let oldData = botResumeData bot
        from    = msgFrom msg
        stuff   = msgCmd msg
    put $ bot { botResumeData = M.insert from stuff oldData }

