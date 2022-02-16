{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lambdabot.Plugin.Telegram.Callback where

import Control.Exception.Lifted ( SomeException (..) )
import Control.Exception.Lifted as E (catch)
import Control.Monad.State (gets, lift)
import Data.List
import Data.List.Split
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Text.EditDistance
import Text.Regex.TDFA

import Lambdabot.Bot
import Lambdabot.Command
import Lambdabot.Config
import Lambdabot.IRC
import Lambdabot.Logging
import Lambdabot.Message
import Lambdabot.Monad
import Lambdabot.Nick
import Lambdabot.Plugin.Core
import Lambdabot.Util

import Lambdabot.Plugin.Telegram.Shared
import Lambdabot.Plugin.Telegram.Message


doTGMSG :: IrcMessage -> Telegram ()
doTGMSG msg = do
  ignored     <- lift $ checkIgnore msg
  commands    <- getConfig commandPrefixes
  if ignored
    then doIGNORE msg
    else mapM_ (doTGMSG' commands (lambdabotName msg) msg) targets
  where
    alltargets = head (ircMsgParams msg)
    targets = map (parseNick (ircMsgServer msg)) $ splitOn "," alltargets

doIGNORE :: IrcMessage -> Telegram ()
doIGNORE = debugM . show

doTGMSG'
  :: [String] -- ^ Commands.
  -> Nick -- ^ My name.
  -> IrcMessage -- ^ IRC Message.
  -> Nick -- ^ Target name.
  -> Telegram ()
doTGMSG' commands myname msg target
    | myname == target
    = let (cmd, params) = splitFirstWord text
      in doPersonalMsg commands msg cmd params
    
    | flip any (":," :: String)
        $ \c -> (fmtNick (ircMsgServer msg) myname ++ [c]) `isPrefixOf` text
    = let Just wholeCmd = maybeCommand (fmtNick (ircMsgServer msg) myname) text
          (cmd, params) = splitFirstWord wholeCmd
      in doPublicMsg commands msg target cmd params
    
    | (commands `arePrefixesOf` text)
    && length text > 1
    && (text !! 1 /= ' ') -- elem of prefixes
    && (not (commands `arePrefixesOf` [text !! 1]) ||
      (length text > 2 && text !! 2 == ' ')) -- ignore @@ prefix, but not the @@ command itself
    = let (cmd, params) = splitFirstWord (dropWhile (==' ') text)
      in doPublicMsg commands msg target cmd params
    
    | otherwise = return () -- contextual messages are not allowed here
    
    where
        text = tail (head (tail (ircMsgParams msg)))

doPersonalMsg
  :: [String]
  -> IrcMessage
  -> String
  -> String
  -> Telegram ()
doPersonalMsg commands msg s r
  | commands `arePrefixesOf` s = doMsg msg (tail s) r who
  | otherwise                  = return () -- contextual messages are not allowed here
  where
    who = nick msg

doPublicMsg
  :: [String] -> IrcMessage -> Nick -> String -> String -> Telegram ()
doPublicMsg commands msg target s r
    | commands `arePrefixesOf` s  = doMsg msg (tail s) r target
    | otherwise                   = doIGNORE msg

--
-- normal commands.
--
-- check privledges, do any spell correction, dispatch, handling
-- possible timeouts.
--
doMsg :: IrcMessage -> String -> String -> Nick -> Telegram ()
doMsg msg cmd rest towhere = do
    ldebug $ "doMsg : nick : " <> fmtNick "" towhere
    let ircmsg = tgIrcPrivMsg (getTgChatId msg) . Text.pack
    allcmds <- lift (gets (Map.keys . ircCommands))
    let ms      = filter (isPrefixOf cmd) allcmds
    e <- getConfig editDistanceLimit
    case ms of
        [s] -> docmd msg towhere rest s                  -- a unique prefix
        _ | cmd `elem` ms -> docmd msg towhere rest cmd  -- correct command (usual case)
        _ | otherwise     -> case closests cmd allcmds of
          (n,[s]) | n < e ,  ms == [] -> docmd msg towhere rest s -- unique edit match
          (n,ss)  | n < e || ms /= []            -- some possibilities
              -> lift . ircmsg $ "Maybe you meant: "++showClean(nub(ms++ss))
          _   -> docmd msg towhere rest cmd         -- no prefix, edit distance too far

docmd :: IrcMessage -> Nick -> [Char] -> String -> Telegram ()
docmd msg towhere rest cmd' = lb $ 
    withCommand cmd'   -- Important.
        (tgIrcPrivMsg (getTgChatId msg)  "Unknown command, try @list")
        (\theCmd -> do
            hasPrivs <- lb (checkPrivs msg)
            -- TODO: handle disabled commands earlier
            -- users should probably see no difference between a
            -- command that is disabled and one that doesn't exist.
            disabled <- elem cmd' <$> getConfig disabledCommands
            let ok = not disabled && (not (privileged theCmd) || hasPrivs)

            response <- if not ok
                then return ["Not enough privileges"]
                else runCommand theCmd msg towhere cmd' rest
                    `E.catch` \exc@SomeException{} ->
                        return ["Plugin `Telegram' failed with: " ++ show exc]
            
            lift $ mapM_ (tgIrcPrivMsg (getTgChatId msg) . Text.pack . expandTab 8) response
        )


closests :: String -> [String] -> (Int,[String])
closests pat ss = Map.findMin m
  where
    m = Map.fromListWith (++) ls
    ls = [ (levenshteinDistance defaultEditCosts pat s, [s]) | s <- ss ]

maybeCommand :: String -> String -> Maybe String
maybeCommand nm text = mrAfter <$> matchM re text
  where
    re :: Regex
    re = makeRegex (nm ++ "[.:,]*[[:space:]]*")
