{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Debug
  ( LogJS (..)
  , Trapper (..)
  , askJSM
  ) where


import           Control.Lens
import           Data.Aeson                  (ToJSON, encode)
import           Data.Kind
import           Data.Text
import           Data.Text.Lazy              (toStrict)
import           Data.Text.Lazy.Encoding
import           Language.Javascript.JSaddle
import           System.IO.Unsafe            (unsafePerformIO)


default (Data.Text.Text)


class LogJS (c :: Type -> Constraint) where
  logJS :: c a => a -> JSM ()

instance LogJS ToJSON where
  logJS a = do
    console <- jsg "console"
    json <- jsg "JSON"
    parsed <- json ^. js1 "parse" (toStrict . decodeUtf8 $ encode a)
    () <$ console ^. js1 "log" parsed

instance LogJS Show where
  logJS a = do
    console <- jsg "console"
    () <$ console ^. js1 "log" (pack $ show a)

instance LogJS ToJSVal where
  logJS a = do
    console <- jsg "console"
    () <$ console ^. js1 "log" (toJSVal a)


class LogJS c => Trapper c where
  trapper :: c a => JSContextRef -> a -> a
  trapper ctx x = unsafePerformIO $ runJSM (x <$ logJS @c x) ctx
  {-# NOINLINE trapper #-}


instance Trapper ToJSON
instance Trapper Show
instance Trapper ToJSVal
