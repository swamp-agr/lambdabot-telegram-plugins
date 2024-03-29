{-# LANGUAGE RecordWildCards #-}
module Lambdabot.Plugin.Telegram.Shared where

import Control.Concurrent.STM (TBQueue, atomically, readTBQueue, writeTBQueue)
import Data.Text (Text)

import Lambdabot.Module (ModuleT)
import Lambdabot.Monad (LB)

-- | Transport type used to communicate between Telegram and Lambdabot.
data Msg = Msg
  { msgChatId :: !Text
  , msgMsgId :: !Text
  , msgMessage :: !Text
  }
  deriving Show

-- | Shared state between Lambdabot and Telegram.
data TelegramState = TelegramState
  { tgInput :: TBQueue Msg
  , tgOutput :: TBQueue Msg
  , tgCurrent :: Int
  , tgBotName :: Text
  }

-- | Lambdabot Monad with embedded Telegram State.
type Telegram = ModuleT TelegramState LB

-- | Read input message from Telegram bot on Lambdabot side.
readInput :: TelegramState -> IO Msg
readInput TelegramState{..} = atomically $ readTBQueue tgInput

-- | Read output message from Lambdabot on Telegram bot side.
readOutput :: TelegramState -> IO Msg
readOutput TelegramState{..} = atomically $ readTBQueue tgOutput

-- | Send input message to Lambdabot from Telegram bot side.
writeInput :: Msg -> TelegramState -> IO ()
writeInput msg TelegramState{..} = atomically $ writeTBQueue tgInput msg

-- | Send output message to Telegram bot from Lambdabot side.
writeOutput :: Msg -> TelegramState -> IO ()
writeOutput msg TelegramState{..} = atomically $ writeTBQueue tgOutput msg
