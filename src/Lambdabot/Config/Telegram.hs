{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Telegram where

import Lambdabot.Config

config "telegramBotName"          [t| String                  |] [| "TelegramLambdabot"         |]
