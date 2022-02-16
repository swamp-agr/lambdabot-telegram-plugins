{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Lambdabot.Plugin.Telegram where

import Codec.Binary.UTF8.String
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Exception.Lifted (finally)
import Control.Monad (void, when)
import Control.Monad.State (gets, lift, liftIO, modify)
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout.Lifted
import Telegram.Bot.Simple

import Lambdabot.Command
import Lambdabot.IRC
import Lambdabot.Monad
import Lambdabot.Module
import Lambdabot.Nick
import Lambdabot.Plugin
import Lambdabot.Util

import Lambdabot.Plugin.Telegram.Bot
import Lambdabot.Plugin.Telegram.Callback
import Lambdabot.Plugin.Telegram.Message
import Lambdabot.Plugin.Telegram.Shared

-- * Lambdabot state

telegramPlugins :: [String]
telegramPlugins = ["telegram"]

telegramPlugin :: Module TelegramState
telegramPlugin = newModule
  { moduleDefState = newTelegramState
  , moduleInit = do
      lb . modify $ \s -> s
        { ircPrivilegedUsers = Set.insert (Nick "telegramrc" "null") (ircPrivilegedUsers s)
        }
      -- register callback for telegram
      registerCallback "TGMSG" doTGMSG
      ldebug "TGMSG callback registered"
      -- note: moduleInit is invoked with exceptions masked
      void . forkUnmasked $ do
        waitForInit
        lockRC
  , moduleCmds = return
    [ (command "telegram")
        { privileged = True
        , help = say "telegram. Start a bot"
        , process = const . lift $ do
            lockRC
            histFile <- lb $ findLBFileForWriting "telegramrc"
            -- FIXME: token should be passed via config or env
            token <- io $ getEnvToken "TELEGRAM_LAMBDABOT_TOKEN"
            tgState <- readMS
            _ <- fork (io $ runTelegramBot token tgState)
            ldebug "telegram bot started"
            _ <- fork (telegramLoop histFile `finally` unlockRC)
            ldebug "telegram loop started"
            return ()
        }
    ]
  }

newTelegramState :: LB TelegramState
newTelegramState = liftIO $ do
  let size = 1000000
      tgCurrent = 0
  tgInput <- newTBQueueIO size
  tgOutput <- newTBQueueIO size
  
  return TelegramState {..}

feed :: Text -> Text -> Telegram ()
feed chatId msg = do
    cmdPrefix <- fmap head (getConfig commandPrefixes)
    let msg' = case Text.unpack msg of
            '>':xs -> cmdPrefix ++ "run " ++ xs
            '!':xs -> xs
            _      -> cmdPrefix ++ dropWhile (== ' ') (Text.unpack msg)
    -- note that `msg'` is unicode, but lambdabot wants utf-8 lists of bytes
    lb . void . timeout (15 * 1000 * 1000) . received $
      makeIrcMessage chatId (Text.pack msg')

handleMsg :: IrcMessage -> Telegram ()
handleMsg msg = do
  let str = case (tail . ircMsgParams) msg of
        []    -> []
        (x:_) -> tail x
  -- str contains utf-8 list of bytes; convert to unicode
  tg <- readMS
  let out = Msg
        { msgChatId = getTgChatId msg
        , msgMessage = (Text.pack . decodeString) str
        }
  ldebug $ "handleMsg : irc : " <> (show msg)
  ldebug $ "handleMsg : out : " <> (show out)
  io $ writeOutput out tg

lockRC :: Telegram ()
lockRC = do
  withMS $ \ tg writ -> do
    when (tgCurrent tg == 0) $ do
      registerServer "telegramrc" handleMsg
      lift $ modify $ \state' ->
        state' { ircPersists = Map.insert "telegramrc" True $ ircPersists state' }
      writ (tg { tgCurrent = tgCurrent tg + 1 })

unlockRC :: Telegram ()
unlockRC = withMS $ \ tg writ -> do
  when (tgCurrent tg == 1) $ unregisterServer "telegramrc"
  writ (tg { tgCurrent = tgCurrent tg - 1})

telegramLoop :: FilePath -> Telegram ()
telegramLoop fp = do
  tg <- readMS
  msg <- io $ readInput tg
  ldebug $ "[DEBUG] : lambdabot : input read : " <> show msg
  let s' = Text.dropWhile isSpace (msgMessage msg)
  when (not (Text.null s')) $ do
    io $ appendFile fp $ Text.unpack $ msgMessage msg
    feed (msgChatId msg) s'
  continue <- lift $ gets (Map.member "telegramrc" . ircPersists)
  when continue $ telegramLoop fp
