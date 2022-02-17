module Lambdabot.Plugin.Telegram.Message where

import Data.Text
import qualified Data.Text as Text

import Lambdabot.IRC
import Lambdabot.Monad
import Lambdabot.Logging

import Lambdabot.Plugin.Telegram.Shared

makeIrcMessage :: Text -> Text -> IrcMessage
makeIrcMessage chatId msg = IrcMessage
  { ircMsgServer  = "telegramrc"
  , ircMsgLBName  = "telegram"
  , ircMsgPrefix  = "null!n=user@" ++ Text.unpack chatId
  , ircMsgCommand = "TGMSG"
  , ircMsgParams  = ["telegram", ":" ++ ((Text.unpack msg)) ]
  }

getTgChatId :: IrcMessage -> Text
getTgChatId = Text.drop 1 . Text.dropWhile (/= '@') . Text.pack . ircMsgPrefix

tgIrcPrivMsg :: Text -> Text -> LB ()
tgIrcPrivMsg chatId txt = send $ makeIrcMessage chatId txt

ldebug :: String -> Telegram ()
ldebug msg = debugM ("lambdabot : " <> show msg)
