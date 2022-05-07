{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Telegram where

import Data.Version
import Lambdabot.Config

config "telegramBotName"           [t| String                  |] [| "TelegramLambdabot"         |]
config "telegramLambdabotVersion"  [t| Version                 |] [| Version [] [] |]
config "muevalBinary"       [t| String                  |] [| "mueval"      |]


-- extensions to enable for the interpreted expression
-- (and probably also L.hs if it doesn't already have these set)
defaultExts :: [String]
defaultExts =
    [ "ImplicitPrelude" -- workaround for bug in hint package
    , "ExtendedDefaultRules"
    ]

configWithMerge [| (++) |] "languageExts"   [t| [String] |] [| defaultExts |]

trustedPkgs :: [String]
trustedPkgs =
    [ "array"
    , "base"
    , "bytestring"
    , "containers"
    , "lambdabot-trusted"
    , "random"
    ]

configWithMerge [| (++) |] "trustedPackages"    [t| [String] |] [| trustedPkgs   |]

config "evalPrefixes"       [t| [String]                |] [| [">"]         |]
config "ghcBinary"          [t| String                  |] [| "ghc"         |]
config "ghciBinary"         [t| String                  |] [| "ghci"        |]
