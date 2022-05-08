module Lambdabot.Plugin.Telegram.Message where

import Data.Text
import qualified Data.Text as Text

import Lambdabot.IRC
import Lambdabot.Monad
import Lambdabot.Logging

import Lambdabot.Plugin.Telegram.Shared

makeIrcMessage :: Text -> Text -> Text -> IrcMessage
makeIrcMessage chatId msgId msg = IrcMessage
  { ircMsgServer  = "telegramrc"
  , ircMsgLBName  = "telegram"
  , ircMsgPrefix  = "null!n=user@" ++ Text.unpack chatId ++ "/" ++ Text.unpack msgId
  , ircMsgCommand = "TGMSG"
  , ircMsgParams  = ["telegram", ":" ++ ((Text.unpack msg)) ]
  }

getTgChatId :: IrcMessage -> Text
getTgChatId
  = Text.takeWhile (/= '/') . Text.drop 1 . Text.dropWhile (/= '@') . Text.pack . ircMsgPrefix

getTgMsgId :: IrcMessage -> Text
getTgMsgId
  = Text.drop 1 . Text.dropWhile (/= '/')
  . Text.drop 1 . Text.dropWhile (/= '@')
  . Text.pack . ircMsgPrefix 

tgIrcPrivMsg :: Text -> Text -> Text -> LB ()
tgIrcPrivMsg chatId msgId txt = send $ makeIrcMessage chatId msgId txt

ldebug :: String -> Telegram ()
ldebug msg = debugM ("lambdabot : " <> show msg)
