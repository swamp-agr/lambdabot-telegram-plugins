{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Lambdabot.Plugin.Telegram.Bot where

import Control.Monad
import Control.Monad.State 
import Data.Char
import Data.Coerce
import Data.Maybe
import qualified Data.Text as Text
import GHC.Generics
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (parseUpdate, updateMessageText)
import qualified Telegram.Bot.Simple.UpdateParser as Update
import Text.Read (readMaybe)

import Lambdabot.Plugin.Telegram.Shared
import Lambdabot.Plugin.Telegram.Bot.Generic

type Model = TelegramState

data Action = SendEverything Msg | SendModule ModuleCmd | SendBack Msg

data ModuleCmd = EvalModule EvalCmd | CheckModule CheckCmd | DjinnModule DjinnCmd

data EvalCmd = Let Msg | Undefine Msg | Run Msg
  deriving (Generic, FromCommand)

data CheckCmd = Check Msg
  deriving (Generic, FromCommand)

data DjinnCmd = Djinn Msg | DjinnAdd Msg | DjinnDel Msg | DjinnEnv Msg | DjinnNames Msg | DjinnClr Msg | DjinnVer Msg
  deriving (Generic, FromCommand)

telegramLambdaBot :: TelegramState -> BotApp Model Action
telegramLambdaBot tgstate = BotApp
  { botInitialModel = tgstate
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update
  -- proxy command
  | isCommand "irc" update = SendEverything <$> updateToMsg update
  -- eval commands
  | isCommand "let" update = SendModule <$> (EvalModule <$> (Let <$> updateToMsg update))
  | isCommand "run" update = SendModule <$> (EvalModule <$> (Run <$> updateToMsg update))
  | isCommand "define" update = SendModule <$> (EvalModule <$> (Let <$> updateToMsg update))
  | isCommand "undefine" update = SendModule <$> (EvalModule <$> (Undefine <$> updateToMsg update))
  -- check commands
  | isCommand "check" update = SendModule <$> (CheckModule <$> (Check <$> updateToMsg update))
  -- djinn commands
  | isCommand "djinn" update = SendModule <$> (DjinnModule <$> (Djinn <$> updateToMsg update))
  | isCommand "djinnadd" update
  = SendModule <$> (DjinnModule <$> (DjinnAdd <$> updateToMsg update))
  | isCommand "djinndel" update
  = SendModule <$> (DjinnModule <$> (DjinnDel <$> updateToMsg update))
  | isCommand "djinnenv" update
  = SendModule <$> (DjinnModule <$> (DjinnEnv <$> updateToMsg update))
  | isCommand "djinnnames" update
  = SendModule <$> (DjinnModule <$> (DjinnNames <$> updateToMsg update))
  | isCommand "djinnclr" update
  = SendModule <$> (DjinnModule <$> (DjinnClr <$> updateToMsg update))
  | isCommand "djinnver" update
  = SendModule <$> (DjinnModule <$> (DjinnVer <$> updateToMsg update))
  | otherwise = Nothing
  where
    isCommand cmd = isJust . parseUpdate (Update.command cmd)
    dropCommand = Text.dropWhile isSpace . Text.dropWhile (not . isSpace)
    intToText = Text.pack . show . coerce @_ @Integer
    updateToMsg upd =
      Msg <$> (fmap intToText . updateChatId) upd <*> (fmap dropCommand . updateMessageText) upd

handlePluginCommand :: FromCommand cmd => cmd -> Model -> Eff Action Model
handlePluginCommand cmd model = model <# do
  liftIO $ writeInput (fromCommand cmd) model
  return ()

handleModuleAction :: ModuleCmd -> Model -> Eff Action Model
handleModuleAction (EvalModule cmd) model = handlePluginCommand cmd model 
handleModuleAction (CheckModule cmd) model = handlePluginCommand cmd model
handleModuleAction (DjinnModule cmd) model = handlePluginCommand cmd model

handleAction :: Action -> Model -> Eff Action Model
handleAction (SendEverything msg) model = model <# do
  liftIO $ writeInput msg model
  return ()
handleAction (SendModule moduleCmd) model = handleModuleAction moduleCmd model
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
