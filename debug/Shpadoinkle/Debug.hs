{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Debug
  ( LogJS (..)
  , Assert (..)
  , Trapper (..)
  , askJSM
  , debug, log, info, warn, table
  , time, timeEnd
  ) where


import           Control.Lens
import           Data.Aeson                  (ToJSON, encode)
import           Data.Kind
import           Data.String
import           Data.Text
import           Data.Text.Lazy              (toStrict)
import           Data.Text.Lazy.Encoding
import           Language.Javascript.JSaddle hiding (startTime)
import           Prelude                     hiding (log)
import           System.IO.Unsafe            (unsafePerformIO)


default (Text)


class LogJS (c :: Type -> Constraint) where
  logJS :: c a => Text -> a -> JSM ()

instance LogJS ToJSON where
  logJS t a = do
    console <- jsg "console"
    json    <- jsg "JSON"
    parsed  <- json ^. js1 "parse" (toStrict . decodeUtf8 $ encode a)
    () <$ console ^. js1 t parsed

instance LogJS Show where
  logJS t a = do
    console <- jsg "console"
    () <$ console ^. js1 t (pack $ show a)

instance LogJS ToJSVal where
  logJS t a = do
    console <- jsg "console"
    () <$ console ^. js1 t (toJSVal a)


debug :: forall c a. LogJS c => c a => a -> JSM ()
debug = logJS @c "debug"


class LogJS c => Trapper c where
  trapper :: c a => JSContextRef -> a -> a
  trapper ctx x = unsafePerformIO $ runJSM (x <$ debug @c x) ctx
  {-# NOINLINE trapper #-}

instance Trapper ToJSON
instance Trapper Show
instance Trapper ToJSVal


newtype TimeLabel = TimeLabel { unTimeLabel :: Text }
  deriving (Eq, Ord, Show, IsString)


time :: TimeLabel -> JSM ()
time (TimeLabel l) = do
  console <- jsg "console"
  () <$ console ^. js1 "time" l


timeEnd :: TimeLabel -> JSM ()
timeEnd (TimeLabel l) = do
  console <- jsg "console"
  () <$ console ^. js1 "timeEnd" l


table :: ToJSON a => [a] -> JSM ()
table = logJS @ToJSON "table"


warn :: forall c a. LogJS c => c a => a -> JSM ()
warn = logJS @c "warn"


info :: forall c a. LogJS c => c a => a -> JSM ()
info = logJS @c "info"


log :: forall c a. LogJS c => c a => a -> JSM ()
log = logJS @c "log"


class Assert (c :: Type -> Constraint) where
  assertLog :: c a => Bool -> a -> JSM ()

instance Assert ToJSON where
  assertLog b x = do
    console <- jsg "console"
    json <- jsg "JSON"
    parsed <- json ^. js1 "parse" (toStrict . decodeUtf8 $ encode x)
    () <$ console ^. js2 "assert" (toJSVal b) parsed

instance Assert Show where
  assertLog b x = do
    console <- jsg "console"
    () <$ console ^. js2 "assert" (toJSVal b) (pack $ show x)

instance Assert ToJSVal where
  assertLog b x = do
    console <- jsg "console"
    () <$ console ^. js2 "assert" (toJSVal b) (toJSVal x)
