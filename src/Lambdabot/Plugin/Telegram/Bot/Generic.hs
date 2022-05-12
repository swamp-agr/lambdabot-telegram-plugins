{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Lambdabot.Plugin.Telegram.Bot.Generic where

import Data.Char
import Data.Text (Text)
import Data.Proxy
import qualified Data.Text as Text
import GHC.Generics
import GHC.TypeLits

import Lambdabot.Plugin.Telegram.Shared

-- | Helper type class used to derive 'FromCommand' via DeriveGeneric extension.
class GFromCommand command where
  gGetMessage :: command proxy -> Msg
  gGetPrefix :: command proxy -> Text

-- Empty data decl
instance GFromCommand V1 where
  gGetMessage x = case x of { }
  gGetPrefix  x = case x of { } 

instance (FromCommand c) => GFromCommand (K1 i c) where
  gGetMessage (K1 x) = getMessage x
  gGetPrefix (K1 x) = getPrefix x

instance (Constructor t, GFromCommand f) => GFromCommand (M1 C t f) where
  gGetMessage (M1 x) = gGetMessage x
  gGetPrefix m@(M1 _) = Text.cons '@' $ toKebabCase $ Text.pack $ conName m

instance (GFromCommand f) => GFromCommand (M1 S t f) where
  gGetMessage (M1 x) = gGetMessage x
  gGetPrefix (M1 x) = gGetPrefix x

instance (GFromCommand f) => GFromCommand (M1 D t f) where
  gGetMessage (M1 x) = gGetMessage x
  gGetPrefix (M1 x) = gGetPrefix x

instance (GFromCommand f, GFromCommand g) => GFromCommand (f :+: g) where
  gGetMessage (L1 x) = gGetMessage x
  gGetMessage (R1 x) = gGetMessage x

  gGetPrefix (L1 x) = gGetPrefix x
  gGetPrefix (R1 x) = gGetPrefix x

instance (GFromCommand f, GFromCommand g) => GFromCommand (f :*: g) where
  gGetMessage (x :*: _y) = gGetMessage x
  gGetPrefix  (x :*: _y) = gGetPrefix  x

class FromCommand command where
  getMessage :: command -> Msg

  default getMessage :: (Generic command, GFromCommand (Rep command)) => command -> Msg
  getMessage x = gGetMessage (from x)

  getPrefix :: command -> Text

  default getPrefix :: (Generic command, GFromCommand (Rep command)) => command -> Text
  getPrefix x = gGetPrefix (from x)

-- | Type class to identify the essence of incoming command and transform it to 'Msg' transport type.
instance FromCommand Msg where
  getMessage = id
  getPrefix = const ""

-- | Transform incoming telegram command into 'Msg'.
fromCommand :: FromCommand command => command -> Msg
fromCommand cmd = old { msgMessage = getPrefix cmd <> " " <> msgMessage old }
    where
      old = getMessage cmd

-- ** Helpers

-- | Helper to transform text into kebab case.
toKebabCase :: Text -> Text
toKebabCase txt =
  let str = Text.unpack txt
      uppers = isUpper <$> str
      indices = zip [0..] uppers :: [(Int, Bool)]
      onlyUpperIndices = filter (/= 0) $ fmap fst $ filter snd indices
      go ix txt' =
        let (begin, end) = Text.splitAt ix txt'
        in Text.concat [ begin, "-", Text.toLower end ]
  in Text.toLower $ foldr go txt onlyUpperIndices

data MaybeWith (modifier :: Modifier) a = MaybeWith a

data Modifier = AtEnd Symbol

instance (KnownSymbol postfix, FromCommand a, modifier ~ 'AtEnd postfix) =>
  FromCommand (MaybeWith modifier a) where

    getMessage (MaybeWith x) = getMessage x
    getPrefix (MaybeWith x) = getPrefix x <> Text.pack (symbolVal (Proxy @postfix))
