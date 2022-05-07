{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
module Lambdabot.Plugin.Telegram where

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
import Data.Version
import qualified Language.Haskell.Exts.Simple as Hs
import System.Directory
import System.Exit
import System.Process
import System.Timeout.Lifted
import Telegram.Bot.Simple

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

lambdabotVersion :: String
lambdabotVersion = VERSION_lambdabot_core

telegramPlugins :: [String]
telegramPlugins = ["telegram"]

-- eval excluded because we provide it by ourselves
customHaskellPlugins :: [String]
customHaskellPlugins =
  [ "check", "djinn", "free", "haddock", "hoogle", "instances"
  , "pl", "pointful", "pretty", "source", "type", "undo", "unmtl"
  ]

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

feed :: Text -> Text -> Telegram ()
feed chatId msg = do
    cmdPrefix <- fmap head (getConfig commandPrefixes)
    let msg' = case Text.unpack msg of
            '>':xs -> cmdPrefix ++ "run " ++ xs
            '!':xs -> xs
            _      -> cmdPrefix ++ dropWhile (== ' ') (Text.unpack msg)
    -- note that `msg'` is unicode, but lambdabot wants utf-8 lists of bytes
    lb . void . timeout (15 * 1000 * 1000) . received $
      makeIrcMessage chatId (Text.pack $ encodeString msg')

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
    io $ appendFile fp $ Text.unpack (msgMessage msg) <> "\n"
    feed (msgChatId msg) s'
  continue <- lift $ gets (Map.member "telegramrc" . ircPersists)
  when continue $ telegramLoop fp

-- ** Eval

args :: String -> String -> [String] -> [String] -> [String]
args load src exts trusted = concat
    [ ["-S"]
    , map ("-s" ++) trusted
    , map ("-X" ++) exts
    , ["--no-imports", "-l", load]
    , ["--expression=" ++ decodeString src]
    , ["+RTS", "-N", "-RTS"]
    ]

isEval :: MonadLB m => String -> m Bool
isEval str = do
    prefixes <- getConfig evalPrefixes
    return (prefixes `arePrefixesWithSpaceOf` str)

dropPrefix :: String -> String
dropPrefix = dropWhile (' ' ==) . drop 2


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

------------------------------------------------------------------------
-- define a new binding

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

-- merge the second module _into_ the first - meaning where merging doesn't
-- make sense, the field from the first will be used
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

moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = do
  copyFile from to
  removeFile from

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

munge, mungeEnc :: String -> String
munge = expandTab 8 . strip (=='\n')
mungeEnc = encodeString . munge

resetCustomL_hs :: MonadLB m => ChatInfo -> m ()
resetCustomL_hs chatInfo = do
    let lhs = getLFilename chatInfo
    p <- findPristine_hs
    contents <- liftIO (readFile p)
    l <- lb (findLBFileForWriting lhs)
    io (writeFile l $ editModuleName chatInfo contents)

-- find Pristine.hs; if not found, we try to install a compiler-specific
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

-- find L.hs; if not found, we copy it from Pristine.hs
findCustomL_hs :: MonadLB m => ChatInfo -> m FilePath
findCustomL_hs chatInfo = do
    let lhs = getLFilename chatInfo
    file <- lb (findLBFileForReading lhs)
    case file of
        -- if L.hs
        Nothing -> resetCustomL_hs chatInfo >> lb (findOrCreateLBFile lhs)
        Just file' -> return file'

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList


data ChatInfo = ChatInfo
  { chatInfoChatId :: !Text
  , chatInfoType   :: !ChatType
  }

data ChatType = Public | Private

renderChatType :: ChatType -> String
renderChatType Public = ""
renderChatType Private = "P"

readChatInfoFromSource :: String -> ChatInfo
readChatInfoFromSource str =
  let prefix = takeWhile (\ch -> isDigit ch || (ch == '-')) str
      mode = case find (== '-') str of
        Nothing -> Public
        Just _  -> Private
      onlyChatId = filter isDigit prefix
  in ChatInfo (Text.pack onlyChatId) mode

dropChatInfoFromSource :: ChatInfo -> String -> String
dropChatInfoFromSource ChatInfo{..} str = dropWhile isSpace $ drop prefixLength str
  where
    prefixLength = Text.length chatInfoChatId + m
    m = case chatInfoType of
      Private -> 1
      Public  -> 0

getDotFilename :: ChatInfo -> String -> FilePath
getDotFilename ChatInfo{..} extension
  = ".L" <> renderChatType chatInfoType <> Text.unpack chatInfoChatId <> "." <> extension

getLFilename :: ChatInfo -> FilePath
getLFilename ChatInfo{..}
  = "L" <> renderChatType chatInfoType <> Text.unpack chatInfoChatId <> ".hs"

editModuleName :: ChatInfo -> String -> String
editModuleName ChatInfo{..} str =
  let moduleName = "L" <> Text.pack (renderChatType chatInfoType) <> chatInfoChatId
      moduleLine = "module " <> moduleName <> " where"
  in (Text.unpack . Text.replace "module L where" moduleLine . Text.pack) str
