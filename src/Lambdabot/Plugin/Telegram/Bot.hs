{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lambdabot.Plugin.Telegram.Bot where

import Control.Monad
import Control.Monad.State 
import Data.Char
import Data.Coerce
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (parseUpdate)
import qualified Telegram.Bot.Simple.UpdateParser as Update
import Text.Read (readMaybe)

import Lambdabot.Plugin.Telegram.Shared
import Lambdabot.Plugin.Telegram.Bot.Generic

type Model = TelegramState

data Action = SendEverything Msg | SendModule ModuleCmd | SendBack Msg

data ModuleCmd
  = EvalModule EvalCmd
  | CheckModule CheckCmd
  | DjinnModule DjinnCmd
  | FreeModule FreeCmd
  | HaddockModule HaddockCmd
  | HoogleModule HoogleCmd
  | InstancesModule InstancesCmd
  | PlModule PlCmd
  | PointfulModule PointfulCmd
  | PrettyModule PrettyCmd
  | SystemModule SystemCmd
  | TypeModule TypeCmd
  | UndoModule UndoCmd
  | UnmtlModule UnmtlCmd
  | VersionModule VersionCmd
  | HelpModule HelpCmd
  | SourceModule SourceCmd

data EvalCmd = Let Msg | Undefine Msg | Run Msg
  deriving (Generic, FromCommand)

data CheckCmd = Check Msg
  deriving (Generic, FromCommand)

data DjinnCmd = Djinn Msg | DjinnAdd Msg | DjinnDel Msg | DjinnEnv Msg | DjinnNames Msg | DjinnClr Msg | DjinnVer Msg
  deriving (Generic, FromCommand)

data FreeCmd = Free Msg
  deriving (Generic, FromCommand)

data HaddockCmd = Index Msg
  deriving (Generic, FromCommand)

data HoogleCmd = Hoogle Msg
  deriving (Generic, FromCommand)

data InstancesCmd = Instances Msg | InstancesImporting Msg
  deriving (Generic, FromCommand)

data PlCmd = Pl Msg | PlResume Msg
  deriving (Generic, FromCommand)

data PointfulCmd = Pointful Msg | Pointy Msg | Repoint Msg | Unpointless Msg | Unpl Msg | Unpf Msg
  deriving (Generic, FromCommand)

data PrettyCmd = Pretty Msg
  deriving (Generic, FromCommand)

data SystemCmd = Listchans Msg | Listmodules Msg | Listservers Msg | List Msg | Echo Msg | Uptime Msg
  deriving (Generic, FromCommand)

data TypeCmd = Type Msg | Kind Msg
  deriving (Generic, FromCommand)

data UndoCmd = Undo Msg | Do Msg
  deriving (Generic, FromCommand)

data UnmtlCmd = Unmtl Msg
  deriving (Generic, FromCommand)

data VersionCmd = Tgversion Msg
  deriving (Generic, FromCommand)

data HelpCmd = Help Msg
  deriving (Generic, FromCommand)

data SourceCmd = Src Msg
  deriving (Generic, FromCommand)

telegramLambdaBot :: TelegramState -> BotApp Model Action
telegramLambdaBot tgstate = BotApp
  { botInitialModel = tgstate
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction TelegramState{..} update
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
  -- free commands
  | isCommand "free" update = SendModule <$> (FreeModule <$> (Free <$> updateToMsg update))
  -- haddock
  | isCommand "index" update = SendModule <$> (HaddockModule <$> (Index <$> updateToMsg update))
  -- hoogle
  | isCommand "hoogle" update
  = SendModule <$> (HoogleModule <$> (Hoogle <$> updateToMsg update))
  -- instances
  | isCommand "instances" update
  = SendModule <$> (InstancesModule <$> (Instances <$> updateToMsg update))
  | isCommand "instancesimporting" update
  = SendModule <$> (InstancesModule <$> (InstancesImporting <$> updateToMsg update))
  -- pl
  | isCommand "pl" update
  = SendModule <$> (PlModule <$> (Pl <$> updateToMsg update))
  | isCommand "plresume" update
  = SendModule <$> (PlModule <$> (PlResume <$> updateToMsg update))
  -- pointful
  | isCommand "pointful" update
  = SendModule <$> (PointfulModule <$> (Pointful <$> updateToMsg update))
  | isCommand "pointy" update
  = SendModule <$> (PointfulModule <$> (Pointy <$> updateToMsg update))
  | isCommand "repoint" update
  = SendModule <$> (PointfulModule <$> (Repoint <$> updateToMsg update))
  | isCommand "unpointless" update
  = SendModule <$> (PointfulModule <$> (Unpointless <$> updateToMsg update))
  | isCommand "unpl" update
  = SendModule <$> (PointfulModule <$> (Unpl <$> updateToMsg update))
  | isCommand "unpf" update
  = SendModule <$> (PointfulModule <$> (Unpf <$> updateToMsg update))
  -- pretty
  | isCommand "pretty" update
  = SendModule <$> (PrettyModule <$> (Pretty <$> updateToMsg update))
  -- system
  -- FIXME: decide about `listchans`, `listservers`
  | isCommand "listmodules" update
  = SendModule <$> (SystemModule <$> (Listmodules <$> updateToMsg update))
  | isCommand "list" update
  = SendModule <$> (SystemModule <$> (List <$> updateToMsg update))
  | isCommand "echo" update
  = SendModule <$> (SystemModule <$> (Echo <$> updateToMsg update))
  | isCommand "uptime" update
  = SendModule <$> (SystemModule <$> (Uptime <$> updateToMsg update))
  -- type
  | isCommand "type" update
  = SendModule <$> (TypeModule <$> (Type <$> updateToMsg update))
  | isCommand "kind" update
  = SendModule <$> (TypeModule <$> (Kind <$> updateToMsg update))
  -- undo
  | isCommand "undo" update
  = SendModule <$> (UndoModule <$> (Undo <$> updateToMsg update))
  | isCommand "do" update
  = SendModule <$> (UndoModule <$> (Do <$> updateToMsg update))
  -- unmtl
  | isCommand "unmtl" update
  = SendModule <$> (UnmtlModule <$> (Unmtl <$> updateToMsg update))
  -- version
  | isCommand "version" update
  = SendModule <$> (VersionModule <$> (Tgversion <$> updateToMsg update))
  -- help
  | isCommand "help" update
  = SendModule <$> (HelpModule <$> (Help <$> updateToMsg update))
  -- source
  -- FIXME: src command is not working properly
  | isCommand "src" update
  = SendModule <$> (SourceModule <$> (Src <$> updateToMsg update))
  | otherwise = Nothing
  where
    isCommand cmd = isJust . parseUpdate (commandWithBotName tgBotName cmd)
    dropCommand = Text.dropWhile isSpace . Text.dropWhile (not . isSpace)
    intToText :: Coercible a Integer => a -> Text
    intToText = Text.pack . show . coerce @_ @Integer
    updateToMsg upd =
      Msg <$> (fmap intToText . updateChatId) upd
          <*> (fmap (intToText . messageMessageId) . extractUpdateMessage) upd
          <*> (fmap dropCommand . join . fmap messageText . extractUpdateMessage) upd

-- FIXME: should it be in `telegram-bot-simple`?
commandWithBotName
  :: Text -- ^ Bot name.
  -> Text -- ^ Command name.
  -> Update.UpdateParser Text
commandWithBotName botname cmdname = do
  t <- textFromAnyMessageType
  case Text.words t of
    (w:ws)| w `elem` ["/" <> cmdname <> "@" <> botname, "/" <> cmdname]
      -> pure (Text.unwords ws)
    _ -> fail "not that command"

textFromAnyMessageType :: Update.UpdateParser Text
textFromAnyMessageType = Update.UpdateParser (extractUpdateMessage >=> messageText)

handlePluginCommand :: FromCommand cmd => cmd -> Model -> Eff Action Model
handlePluginCommand cmd model = model <# do
  liftIO $ writeInput (fromCommand cmd) model
  return ()

handleModuleAction :: ModuleCmd -> Model -> Eff Action Model
handleModuleAction (EvalModule cmd) model = handlePluginCommand cmd model 
handleModuleAction (CheckModule cmd) model = handlePluginCommand cmd model
handleModuleAction (DjinnModule cmd) model = handlePluginCommand cmd model
handleModuleAction (FreeModule cmd) model = handlePluginCommand cmd model
handleModuleAction (HaddockModule cmd) model = handlePluginCommand cmd model
handleModuleAction (HoogleModule cmd) model = handlePluginCommand cmd model
handleModuleAction (InstancesModule cmd) model = handlePluginCommand cmd model
handleModuleAction (PlModule cmd) model = handlePluginCommand cmd model
handleModuleAction (PointfulModule cmd) model = handlePluginCommand cmd model
handleModuleAction (PrettyModule cmd) model = handlePluginCommand cmd model
handleModuleAction (SystemModule cmd) model = handlePluginCommand cmd model
handleModuleAction (TypeModule cmd) model = handlePluginCommand cmd model
handleModuleAction (UndoModule cmd) model = handlePluginCommand cmd model
handleModuleAction (UnmtlModule cmd) model = handlePluginCommand cmd model
handleModuleAction (VersionModule cmd) model = handlePluginCommand cmd model
handleModuleAction (HelpModule cmd) model = case (msgMessage $ getMessage cmd) of
  "" -> model <# do
    pure
      $ SendBack
      $ Msg { msgChatId = msgChatId $ getMessage cmd
            , msgMsgId = msgMsgId $ getMessage cmd
            , msgMessage = helpCmd
            }
  _  -> handlePluginCommand cmd model
handleModuleAction (SourceModule cmd) model = handlePluginCommand cmd model

handleAction :: Action -> Model -> Eff Action Model
handleAction (SendEverything msg) model = model <# do
  liftIO $ writeInput msg model
  return ()
handleAction (SendModule moduleCmd) model = handleModuleAction moduleCmd model
handleAction (SendBack msg) model = model <# do
  let Msg chatIdText msgIdText response = msg
      parseChatId = fmap ChatId . readMaybe . Text.unpack
      parseMsgId  = fmap MessageId . readMaybe . Text.unpack
      mchatId = parseChatId chatIdText
      mreplyMessageId = parseMsgId msgIdText
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
            , sendMessageProtectContent        = Nothing
            , sendMessageReplyToMessageId      = mreplyMessageId
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

helpCmd :: Text
helpCmd = "Lambdabot for Telegram provides following plugins:\n\
\\n\
\telegram check djinn free haddock hoogle instances pl pointful pretty source system type undo unmtl\n\
\\n\
\telegram plugin has following commands:\n\
\\n\
\- /version - version/source. Report the version and git repo of this bot\n\
\- /run - run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!\n\
\- /let - let <x> = <e>. Add a binding.\n\
\- /define - let <x> = <e>. Add a binding.\n\
\- /undefine - undefine. Reset evaluator local bindings.\n\
\\n\
\check plugin has following command:\n\
\\n\
\- /check - check <expr>. You have QuickCheck and 3 seconds. Prove something.\n\
\\n\
\djinn plugin has following commands:\n\
\\n\
\- /djinn - djinn <type>. Generates Haskell code from a type.\n\
\- /djinnadd - djinn-add <expr>. Define a new function type or type synonym.\n\
\- /djinndel - djinn-del <ident>. Remove a symbol from the environment.\n\
\- /djinnenv - Show the current djinn environment.\n\
\- /djinnnames - Show the current djinn environment, compactly.\n\
\- /djinnclr - Reset the djinn environment.\n\
\- /djinnver - Show current djinn version.\n\
\\n\
\free plugin has following command:\n\
\\n\
\- /free - free <ident>. Generate theorems for free.\n\
\\n\
\haddock plugin has following command:\n\
\\n\
\- /index - index <ident>. Returns the Haskell modules in which <ident> is defined.\n\
\\n\
\hoogle plugin has following command:\n\
\\n\
\- /hoogle - hoogle <expr>. Haskell API Search for either names, or types.\n\
\\n\
\instances plugin has following commands:\n\
\\n\
\- /instances - instances <typeclass>. Fetch the instances of a typeclass.\n\
\- /instancesimporting - instancesimporting [<module> [<module> [<module...]]] <typeclass>. Fetch the instances of a typeclass, importing specified modules first.\n\
\\n\
\pl plugin has following command:\n\
\\n\
\- /pl - pointless <expr>. Play with pointfree code.\n\
\\n\
\pointful plugin has following commands:\n\
\\n\
\- /pointy - pointful <expr>. Make code pointier.\n\
\- /repoint - pointful <expr>. Make code pointier.\n\
\- /unpointless - pointful <expr>. Make code pointier.\n\
\- /unpl - pointful <expr>. Make code pointier.\n\
\- /unpf - pointful <expr>. Make code pointier.\n\
\\n\
\pretty plugin has following commands:\n\
\\n\
\- /pretty - pretty <expr>. Display haskell code in a pretty-printed manner\n\
\\n\
\type plugin has following commands:\n\
\\n\
\- /type - type <expr>. Return the type of a value.\n\
\- /kind - kind <type>. Return the kind of a type.\n\
\\n\
\source plugin has following commands:\n\
\- /src - src <id>. Display the implementation of a standard function.\n\
\\n\
\undo plugin has following commands:\n\
\\n\
\- /undo - undo <expr>. Translate do notation to Monad operators.\n\
\- /do - do <expr>. Translate Monad operators to do notation.\n\
\\n\
\unmtl has following commands:\n\
\\n\
\- /unmtl - unroll mtl monads.\n\
\\n\
\Other commands:\n\
\- /help - shows this help.\n\
\- /version - version/source. Report the version and git repo of this bot\n\
\\n\
\All plugins are independent from each other, i.e. have their own state or use different programs under the hood."

