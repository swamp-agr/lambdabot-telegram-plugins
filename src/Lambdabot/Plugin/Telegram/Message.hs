module Lambdabot.Plugin.Telegram.Message where

import Data.Text
import qualified Data.Text as Text

import Lambdabot.IRC
import Lambdabot.Monad
import Lambdabot.Logging

import Lambdabot.Plugin.Telegram.Shared

-- * IRC Messaging

-- | IRC communicating model consists of the core and plugins which are sending messages to each other.
--
-- @Telegram module <---> Lambdabot core module <------> Haskell/Telegram module@
--
-- In order to pass Telegram-related necessary information for responding, we are embedding Telegram metadata into 'IrcMessage' inside 'ircMsgPrefix': @"null!n=user\@" + chatId + "/" + msgId@
--
makeIrcMessage :: Text -> Text -> Text -> IrcMessage
makeIrcMessage chatId msgId msg = IrcMessage
  { ircMsgServer  = "telegramrc"
  , ircMsgLBName  = "telegram"
  , ircMsgPrefix  = "null!n=user@" ++ Text.unpack chatId ++ "/" ++ Text.unpack msgId
  , ircMsgCommand = "TGMSG"
  , ircMsgParams  = ["telegram", ":" ++ ((Text.unpack msg)) ]
  }

-- | To extract Telegram @chatId@ from 'IrcMessage'.
getTgChatId :: IrcMessage -> Text
getTgChatId
  = Text.takeWhile (/= '/') . Text.drop 1 . Text.dropWhile (/= '@') . Text.pack . ircMsgPrefix

-- | To extract Telegram @msgId@ from 'IrcMessage'.
getTgMsgId :: IrcMessage -> Text
getTgMsgId
  = Text.drop 1 . Text.dropWhile (/= '/')
  . Text.drop 1 . Text.dropWhile (/= '@')
  . Text.pack . ircMsgPrefix 

-- | Send privileged IRC Message across modules.
tgIrcPrivMsg :: Text -> Text -> Text -> LB ()
tgIrcPrivMsg chatId msgId txt = send $ makeIrcMessage chatId msgId txt

-- | Debug helper function.
ldebug :: String -> Telegram ()
ldebug msg = debugM ("lambdabot : " <> show msg)
