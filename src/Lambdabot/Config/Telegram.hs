{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Telegram where

import Data.Version
import Lambdabot.Config

-- | Telegram Bot Name.
config "telegramBotName"           [t| String                  |] [| "TelegramLambdabot"         |]

-- | Telegram Lambdabot Version.
config "telegramLambdabotVersion"  [t| Version                 |] [| Version [] [] |]

-- | Path to @mueval@ executable.
config "muevalBinary"       [t| String                  |] [| "mueval"      |]


-- | Extensions to enable for the interpreted expression
-- (and probably also @L.hs@ if it doesn't already have these set)
--
-- Some of these settings were propagated from 'lambdabot-haskell-plugins'
-- because @eval@ plugin internals were hidden.
defaultExts :: [String]
defaultExts =
    [ "ImplicitPrelude" -- workaround for bug in hint package
    , "ExtendedDefaultRules"
    , "TypeApplications"
    , "MagicHash"
    ]

-- | Language extensions used by Telegram Lambdabot: combination of predefined 'defaultExts' and user-defined ones.
configWithMerge [| (++) |] "languageExts"   [t| [String] |] [| defaultExts |]

-- | Predefined packages that are mandatory.
trustedPkgs :: [String]
trustedPkgs =
    [ "array"
    , "base"
    , "bytestring"
    , "containers"
    , "lambdabot-trusted"
    , "random"
    ]

-- | Set of trusted packages. Combination of predefined 'trustedPkgs' and user-defined ones.
configWithMerge [| (++) |] "trustedPackages"    [t| [String] |] [| trustedPkgs   |]

-- | Command prefixes for fork of @eval@ plugin.
config "evalPrefixes"       [t| [String]                |] [| [">"]         |]

-- | Command to invoke @ghc@.
config "ghcBinary"          [t| String                  |] [| "ghc"         |]

-- | Command to invoke @ghci@.
config "ghciBinary"         [t| String                  |] [| "ghci"        |]
