{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Lambdabot.Plugin.Telegram.Bot where

import Control.Monad
import Control.Monad.State 
import Data.Coerce
import Data.Maybe
import qualified Data.Text as Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (parseUpdate, updateMessageText)
import qualified Telegram.Bot.Simple.UpdateParser as Update
import Text.Read (readMaybe)

import Lambdabot.Plugin.Telegram.Shared

type Model = TelegramState

data Action = SendEverything Msg | SendBack Msg

telegramLambdaBot :: TelegramState -> BotApp Model Action
telegramLambdaBot tgstate = BotApp
  { botInitialModel = tgstate
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update
  | isJust $ parseUpdate (Update.command "irc") update
  = SendEverything
    <$> (Msg
         <$> (fmap intToText . updateChatId) update
         <*> (fmap (Text.drop 5) . updateMessageText) update
        )
  | otherwise = Nothing
  where
    intToText = Text.pack . show . coerce @_ @Integer

handleAction :: Action -> Model -> Eff Action Model
handleAction (SendEverything msg) model = model <# do
  liftIO $ writeInput msg model
  return ()
handleAction (SendBack msg) model = model <# do
  let Msg chatIdText response = msg
      parseChatId = fmap ChatId . readMaybe . Text.unpack
      mchatId = parseChatId chatIdText
  case mchatId of
    Nothing -> pure ()
    Just tgchatId -> do
      let req = SendMessageRequest
            { sendMessageChatId                = SomeChatId tgchatId
            , sendMessageText                  = response
            , sendMessageParseMode             = Nothing
            , sendMessageEntities              = Nothing
            , sendMessageDisableWebPagePreview = Nothing
            , sendMessageDisableNotification   = Nothing
            , sendMessageReplyToMessageId      = Nothing
            , sendMessageAllowSendingWithoutReply = Nothing
            , sendMessageReplyMarkup           = Nothing
            }
      _ <- liftClientM (sendMessage req)
      pure ()
  
runTelegramBot :: Token -> TelegramState -> IO ()
runTelegramBot token tgstate = do
  env <- defaultTelegramClientEnv token
  botActionFun <- startBotAsync (telegramLambdaBot tgstate) env
  forever $ do
    response <- readOutput tgstate
    botActionFun (SendBack response)
