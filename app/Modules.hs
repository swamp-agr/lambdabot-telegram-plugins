{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Lambdabot.Main

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
import Lambdabot.Plugin.Haskell
import Lambdabot.Plugin.Telegram

modulesInfo :: Modules
modulesInfo = $(modules $ corePlugins ++ customHaskellPlugins ++ telegramPlugins)
