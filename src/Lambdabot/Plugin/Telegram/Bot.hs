{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Lambdabot.Plugin.Telegram.Bot where

import Control.Monad
import Control.Monad.State 
import Data.Char
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

data Action = SendEverything Msg | SendModule ModuleCmd | SendBack Msg

data ModuleCmd = Eval EvalCmd

data EvalCmd = Let Msg | Undefine Msg | Run Msg

-- FIXME: generalise via typeclass and generic

getPrefix :: EvalCmd -> Text.Text
getPrefix = \case
  Let _ -> "@let"
  Undefine _ -> "@undefine"
  Run _ -> "@run"

-- FIXME: generalise via typeclass and generic

getMsg :: EvalCmd -> Msg
getMsg = \case
  Let cmd -> cmd
  Undefine cmd -> cmd
  Run cmd -> cmd

prependEvalCommand :: EvalCmd -> Msg
prependEvalCommand cmd = old { msgMessage = getPrefix cmd <> " " <> msgMessage old }
  where
    old = getMsg cmd

telegramLambdaBot :: TelegramState -> BotApp Model Action
telegramLambdaBot tgstate = BotApp
  { botInitialModel = tgstate
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update
  | isCommand "irc" update = SendEverything <$> updateToMsg update
  | isCommand "let" update = SendModule <$> (Eval <$> (Let <$> updateToMsg update))
  | isCommand "run" update = SendModule <$> (Eval <$> (Run <$> updateToMsg update))
  | isCommand "define" update = SendModule <$> (Eval <$> (Let <$> updateToMsg update))
  | isCommand "undefine" update = SendModule <$> (Eval <$> (Undefine <$> updateToMsg update))
  | otherwise = Nothing
  where
    isCommand cmd = isJust . parseUpdate (Update.command cmd)
    dropCommand = Text.dropWhile isSpace . Text.dropWhile (not . isSpace)
    intToText = Text.pack . show . coerce @_ @Integer
    updateToMsg upd =
      Msg <$> (fmap intToText . updateChatId) upd <*> (fmap dropCommand . updateMessageText) upd

handleEval :: EvalCmd -> Model -> Eff Action Model
handleEval evalCmd model = model <# do
  liftIO $ writeInput (prependEvalCommand evalCmd) model
  return ()

handleModuleAction :: ModuleCmd -> Model -> Eff Action Model
handleModuleAction (Eval cmd) model = handleEval cmd model 

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
