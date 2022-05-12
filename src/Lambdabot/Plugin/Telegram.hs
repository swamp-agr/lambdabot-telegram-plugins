{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
module Lambdabot.Plugin.Telegram
  ( -- * Lambdabot state

    lambdabotVersion
  , telegramPlugins
  , telegramPlugin
  , customHaskellPlugins
  , newTelegramState
  , feed
  , handleMsg
  , lockRC
  , unlockRC

  -- * Eval

  -- $eval
  , args, isEval, dropPrefix, runGHC, define, mergeModules, moduleProblems, moveFile, customComp, resetCustomL_hs, findPristine_hs, findCustomL_hs

  -- $chatType
  , ChatInfo(..), ChatType(..), renderChatType, readChatInfoFromSource, dropChatInfoFromSource, getDotFilename, getLFilename, editModuleName
  ) where

import Codec.Binary.UTF8.String
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Exception.Lifted (SomeException, try, finally)
import Control.Monad (void, when)
import Control.Monad.State (gets, lift, liftIO, modify)
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Data.Version
import qualified Language.Haskell.Exts.Simple as Hs
import System.Directory
import System.Exit
import System.Process
import System.Timeout.Lifted
import Telegram.Bot.Simple
import Text.Pretty.Simple (pStringNoColor)

import Lambdabot.Command
import Lambdabot.Config.Telegram
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

-- | Lambdabot version from 'lambdabot-core' package.
lambdabotVersion :: String
lambdabotVersion = VERSION_lambdabot_core

-- | Exported plugin(s).
telegramPlugins :: [String]
telegramPlugins = ["telegram"]

-- | Eval excluded because we provide it by ourselves.
customHaskellPlugins :: [String]
customHaskellPlugins =
  [ "check", "djinn", "free", "haddock", "hoogle", "instances"
  , "pl", "pointful", "pretty", "source", "type", "undo", "unmtl"
  ]

-- | Telegram plugin for Lambdabot.
-- Here we redefined @eval@ plugin to provide multiple sandboxes for different chats.
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
    , (command "tgversion")
        { help = say $
            "version/source. Report version(s) and git repo of this bot."
        , process = const $ do
            ver <- getConfig telegramLambdabotVersion
            say $ unlines $
              [ "telegram-lambdabot v."
                <> showVersion ver
                <> ". git clone https://github.com/swamp-agr/lambdabot-telegram-plugins.git"
              , "lambdabot-core v."
                <> lambdabotVersion
                <> ". git clone https://github.com/lambdabot/lambdabot.git"
              ]
        }
    , (command "run")
        { help = say "run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!"
        , process = lim80 . runGHC
        }
    , (command "let")
        { aliases = ["define"] -- because @define always gets "corrected" to @undefine
        , help = say "let <x> = <e>. Add a binding"
        , process = lim80 . define
        }
    , (command "undefine")
        { help = say "undefine. Reset evaluator local bindings"
        , process = \s -> 
            let chatInfo = readChatInfoFromSource s
                s' = dropChatInfoFromSource chatInfo s
             in 
            if null s'
              then resetCustomL_hs chatInfo >> say "Undefined."
              else say "There's currently no way to undefine just one thing.  Say @undefine (with no extra words) to undefine everything."
        }
    ]
  }

-- | Initialise 'TelegramState' from Lambdabot config and with defaults. Current defaults are:
-- 
-- * Input queue size: 1000000.
-- * Output queue size: 1000000.
newTelegramState :: LB TelegramState
newTelegramState = do
  tgBotName <- Text.pack <$> getConfig telegramBotName
  liftIO $ do
    putStrLn $ " bot name is : " <> Text.unpack (tgBotName)
    let size = 1000000
        tgCurrent = 0
    tgInput <- newTBQueueIO size
    tgOutput <- newTBQueueIO size
  
    return TelegramState {..}

-- | Commands preprocessing. Commands started with @>@, @!@ would be replaced
-- with @<commadPrefix (from Lambdabot config)> + "run "@.
-- Resulted command would be passed to IRC @system@ plugin.
feed :: Text -> Text -> Text -> Telegram ()
feed chatId msgId msg = do
    cmdPrefix <- fmap head (getConfig commandPrefixes)
    let msg' = case Text.unpack msg of
            '>':xs -> cmdPrefix ++ "run " ++ xs
            '!':xs -> xs
            _      -> cmdPrefix ++ dropWhile (== ' ') (Text.unpack msg)
    -- note that `msg'` is unicode, but lambdabot wants utf-8 lists of bytes
    lb . void . timeout (15 * 1000 * 1000) . received $
      makeIrcMessage chatId msgId (Text.pack $ encodeString msg')

-- | Transcoding the response from IRC @system@ plugin and sending message back to Telegram. 
handleMsg :: IrcMessage -> Telegram ()
handleMsg msg = do
  let str = case (tail . ircMsgParams) msg of
        []    -> []
        (x:_) -> tail x
  -- str contains utf-8 list of bytes; convert to unicode
  tg <- readMS
  let out = Msg
        { msgChatId = getTgChatId msg
        , msgMsgId = getTgMsgId msg
        , msgMessage = (TL.toStrict . pStringNoColor . decodeString) str
        }
  ldebug $ "handleMsg : irc : " <> (show msg)
  ldebug $ "handleMsg : out : " <> (show out)
  io $ writeOutput out tg

-- | Register @telegram@ plugin in lambdabot core.
lockRC :: Telegram ()
lockRC = do
  withMS $ \ tg writ -> do
    when (tgCurrent tg == 0) $ do
      registerServer "telegramrc" handleMsg
      lift $ modify $ \state' ->
        state' { ircPersists = Map.insert "telegramrc" True $ ircPersists state' }
      writ (tg { tgCurrent = tgCurrent tg + 1 })

-- | Unregister @telegram@ plugin in lambdabot core.
unlockRC :: Telegram ()
unlockRC = withMS $ \ tg writ -> do
  when (tgCurrent tg == 1) $ unregisterServer "telegramrc"
  writ (tg { tgCurrent = tgCurrent tg - 1})

-- | The main loop process.
-- Constantly read the messages from Telegram and passing them to IRC core.
telegramLoop :: FilePath -> Telegram ()
telegramLoop fp = do
  tg <- readMS
  msg <- io $ readInput tg
  ldebug $ "[DEBUG] : lambdabot : input read : " <> show msg
  let s' = Text.dropWhile isSpace (msgMessage msg)
  when (not (Text.null s')) $ do
    io $ appendFile fp $ Text.unpack (msgMessage msg) <> "\n"
    feed (msgChatId msg) (msgMsgId msg) s'
  continue <- lift $ gets (Map.member "telegramrc" . ircPersists)
  when continue $ telegramLoop fp

-- ** Eval

-- $eval
-- Functions above came from @eval@ plugin from @lambdabot-haskell-plugins@ package.
-- Instead of registering multiple "servers" for each Telegram chat and introducing new entities to manage multiple "servers",
-- we decided to keep a single "server" and to modify "sandboxes".
-- Sandbox is a basically single file "L.hs" that populated once IRC received "@let" command.
-- For every chat used exactly the same file. It means that it was possible to share all definitions across different Telegram chats.
-- To overcome these limitations we decided to create a separate file and associate it with a chat.

-- | Concatenate all input into list of strings to pass to GHC:
--
-- @args filesToLoad sourceExpressionToEvaluate ghcExtensions trustedPackages@
args :: String -> String -> [String] -> [String] -> [String]
args load src exts trusted = concat
    [ ["-S"]
    , map ("-s" ++) trusted
    , map ("-X" ++) exts
    , ["--no-imports", "-l", load]
    , ["--expression=" ++ decodeString src]
    , ["+RTS", "-N", "-RTS"]
    ]

-- | Determine whether command belongs to @eval@ plugin or not.
isEval :: MonadLB m => String -> m Bool
isEval str = do
    prefixes <- getConfig evalPrefixes
    return (prefixes `arePrefixesWithSpaceOf` str)

-- | Drop command prefix.
dropPrefix :: String -> String
dropPrefix = dropWhile (' ' ==) . drop 2

-- | Represents "run" command. Actually run GHC.
-- Response would be handled separately via callbacks.
runGHC :: MonadLB m => String -> m String
runGHC src' = do
    let chatInfo = readChatInfoFromSource src'
        src = dropChatInfoFromSource chatInfo src'

    load    <- findCustomL_hs chatInfo
    binary  <- getConfig muevalBinary
    exts    <- getConfig languageExts
    trusted <- getConfig trustedPackages
    (_,out,err) <- io (readProcessWithExitCode binary (args load src exts trusted) "")
    case (out,err) of
        ([],[]) -> return "Terminated\n"
        _       -> do
            let o = mungeEnc out
                e = mungeEnc err
            return $ case () of {_
                | null o && null e -> "Terminated\n"
                | null o           -> e
                | otherwise        -> o
            }

-- | "define" command. Define a new binding. It would be stored in corresponding sandbox.
define :: MonadLB m => String -> m String
define [] = return "Define what?"
define src' = do
    exts <- getConfig languageExts
    let chatInfo = readChatInfoFromSource src'
        src = dropChatInfoFromSource chatInfo src'
        mode = Hs.defaultParseMode{ Hs.extensions = map Hs.classifyExtension exts }
    case Hs.parseModuleWithMode mode (decodeString src) of
        Hs.ParseOk srcModule -> do
            l <- findCustomL_hs chatInfo
            res <- io (Hs.parseFile l)
            case res of
                Hs.ParseFailed loc err -> return (Hs.prettyPrint loc ++ ':' : err)
                Hs.ParseOk lModule -> do
                    let merged = mergeModules lModule srcModule
                    case moduleProblems merged of
                        Just msg -> return msg
                        Nothing  -> customComp chatInfo merged
        Hs.ParseFailed _loc err -> return ("Parse failed: " ++ err)

-- | Merge the second module _into_ the first - meaning where merging doesn't
-- make sense, the field from the first will be used.
mergeModules :: Hs.Module -> Hs.Module -> Hs.Module
mergeModules (Hs.Module  head1  exports1 imports1 decls1)
             (Hs.Module _head2 _exports2 imports2 decls2)
    = Hs.Module head1 exports1
        (mergeImports imports1 imports2)
        (mergeDecls   decls1   decls2)
    where
        mergeImports x y = nub' (sortBy (comparing Hs.importModule) (x ++ y))
        mergeDecls x y = sortBy (comparing funcNamesBound) (x ++ y)

        -- this is a very conservative measure... we really only even care about function names,
        -- because we just want to sort those together so clauses can be added in the right places
        -- TODO: find out whether the [Hs.Match] can contain clauses for more than one function (e,g. might it be a whole binding group?)
        funcNamesBound (Hs.FunBind ms) = nub $ sort [ n | Hs.Match n _ _ _ <- ms]
        funcNamesBound _ = []
-- we simply do not care about XML cases
mergeModules _ _ = error "Not supported module met"

-- | Import validations.
moduleProblems :: Hs.Module -> Maybe [Char]
moduleProblems (Hs.Module _head pragmas _imports _decls)
    | safe `notElem` langs  = Just "Module has no \"Safe\" language pragma"
    | trusted `elem` langs  = Just "\"Trustworthy\" language pragma is set"
    | otherwise             = Nothing
    where
        safe    = Hs.name "Safe"
        trusted = Hs.name "Trustworthy"
        langs = concat [ ls | Hs.LanguagePragma ls <- pragmas ]
-- we simply do not care about XML cases
moduleProblems _ = error "Not supported module met"

-- | Helper for sandboxes. Used to move temporary file.
moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = do
  copyFile from to
  removeFile from

-- | Custom compilation of temporary files for binding. Used as part of "define" command.
customComp :: MonadLB m => ChatInfo -> Hs.Module -> m String
customComp chatInfo src = do
    -- calculate temporary filename for source, interface and compiled library
    let hs = getDotFilename chatInfo "hs"
        hi = getDotFilename chatInfo "hi"
        lib = getDotFilename chatInfo "o"
        lhs = getLFilename chatInfo
    -- Note we copy to .L.hs, not L.hs. This hides the temporary files as dot-files
    io (writeFile hs (Hs.prettyPrint src))

    -- and compile .L.hs
    -- careful with timeouts here. need a wrapper.
    trusted <- getConfig trustedPackages
    let ghcArgs = concat
            [ ["-O", "-v0", "-c", "-Werror", "-fpackage-trust"]
            , concat [["-trust", pkg] | pkg <- trusted]
            , [hs]
            ]
    ghc <- getConfig ghcBinary
    (c, o',e') <- io (readProcessWithExitCode ghc ghcArgs "")
    -- cleanup, 'try' because in case of error the files are not generated
    _ <- io (try (removeFile hi) :: IO (Either SomeException ()))
    _ <- io (try (removeFile lib)  :: IO (Either SomeException ()))

    case (mungeEnc o', mungeEnc e') of
        ([],[]) | c /= ExitSuccess -> do
                    io (removeFile hs)
                    return "Error."
                | otherwise -> do
                    l <- lb (findLBFileForWriting lhs)
                    io (moveFile hs l)
                    return "Defined."
        (ee,[]) -> return ee
        (_ ,ee) -> return ee

-- | Reset sandbox. Associated "L.hs" file would be reset to defaults.
resetCustomL_hs :: MonadLB m => ChatInfo -> m ()
resetCustomL_hs chatInfo = do
    let lhs = getLFilename chatInfo
    p <- findPristine_hs
    contents <- liftIO (readFile p)
    l <- lb (findLBFileForWriting lhs)
    io (writeFile l $ editModuleName chatInfo contents)

-- | Find "Pristine.hs"; if not found, we try to install a compiler-specific
-- version from lambdabot's data directory, and finally the default one.
findPristine_hs :: MonadLB m => m FilePath
findPristine_hs = do
    p <- lb (findLBFileForReading "Pristine.hs")
    case p of
        Nothing -> do
            p' <- lb (findOrCreateLBFile "Pristine.hs")
            p0 <- lb (findLBFileForReading ("Pristine.hs." ++ show (__GLASGOW_HASKELL__ :: Integer)))
            p0' <- case p0 of
                Nothing -> lb (findLBFileForReading "Pristine.hs.default")
                p0' -> return p0'
            case p0' of
                Just p0'' -> do
                    p'' <- lb (findLBFileForWriting "Pristine.hs")
                    io (copyFile p0'' p'')
                _ -> return ()
            return p'
        Just p' -> return p'

-- | Find associated with a chat "L.hs" file; if not found, we copy it from "Pristine.hs".
findCustomL_hs :: MonadLB m => ChatInfo -> m FilePath
findCustomL_hs chatInfo = do
    let lhs = getLFilename chatInfo
    file <- lb (findLBFileForReading lhs)
    case file of
        -- if L.hs
        Nothing -> resetCustomL_hs chatInfo >> lb (findOrCreateLBFile lhs)
        Just file' -> return file'

-- $chatType
-- Since Lambdabot is passing 'String' inside one plugin (see 'process' for more details), Telegram chat information rendered as 'String' and attached to every command to pass between commands and callbacks inside a plugin.

-- | 'ChatInfo' represents an associated Telegram chat. Currently supports private and public chats.
data ChatInfo = ChatInfo
  { chatInfoChatId :: !Text
  , chatInfoType   :: !ChatType
  }

-- | 'ChatType' represents whether chat is public or private.
data ChatType = Public | Private

-- | Since it's not possible to define module with "L-1000" name and private chats in Telegram are usually represented by negative digits, we simple encode it with a character.
renderChatType :: ChatType -> String
renderChatType Public = ""
renderChatType Private = "P"

-- | Read 'ChatInfo' from command string.
readChatInfoFromSource :: String -> ChatInfo
readChatInfoFromSource str =
  let prefix = takeWhile (/= '|') str
      mode = case find (== '-') str of
        Nothing -> Public
        Just _  -> Private
      onlyChatId = filter isDigit prefix
  in ChatInfo (Text.pack onlyChatId) mode

-- | Drop 'ChatInfo' from command string. Original command is returned.
dropChatInfoFromSource :: ChatInfo -> String -> String
dropChatInfoFromSource ChatInfo{..} str = drop 1 . dropWhile (/= '|') $ drop prefixLength str
  where
    prefixLength = Text.length chatInfoChatId + m
    m = case chatInfoType of
      Private -> 1
      Public  -> 0

-- | Generate temporary filename for given 'ChatInfo' and file extension.
getDotFilename :: ChatInfo -> String -> FilePath
getDotFilename ChatInfo{..} extension
  = ".L" <> renderChatType chatInfoType <> Text.unpack chatInfoChatId <> "." <> extension

-- | Generate "L.hs" filename for given 'ChatInfo'.
getLFilename :: ChatInfo -> FilePath
getLFilename ChatInfo{..}
  = "L" <> renderChatType chatInfoType <> Text.unpack chatInfoChatId <> ".hs"

-- | Replace @module L where@ with @module L\<chatId\> where@ where @\<chatId\>@ is a telegram chat ID.
editModuleName :: ChatInfo -> String -> String
editModuleName ChatInfo{..} str =
  let moduleName = "L" <> Text.pack (renderChatType chatInfoType) <> chatInfoChatId
      moduleLine = "module " <> moduleName <> " where"
  in (Text.unpack . Text.replace "module L where" moduleLine . Text.pack) str

munge, mungeEnc :: String -> String
munge = expandTab 8 . strip (=='\n')
mungeEnc = encodeString . munge

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList
