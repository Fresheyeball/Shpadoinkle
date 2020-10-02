{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


{-|
  This module exposes the browser's native console logging and debugging features,
  including underutilized features such as time measurement, table displays, and assertions.
-}


module Shpadoinkle.Console (
  -- * Classes
  LogJS (..), Assert (..), Trapper (..), askJSM
  -- * Native methods
  -- ** Log levels
  , log, debug, info, warn
  -- ** Fancy display
  , table
  -- ** Time Measurement
  , TimeLabel(..), time, timeEnd
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

{-|
   'LogJS' is the base class for logging to the browser console.
   Browser consoles contain rich tooling for exploring JavaScript objects,
   DOM nodes, and much more. To take advantage of these native features, we
   need to choose how we are going to log. The 'LogJS' class is intended to
   be used in conjunction with 'TypeApplications'.

   @
   data Person = Person { first :: String, last :: String, age :: Int } deriving (Generic, ToJSON)
   main = logJS @ToJSON "log" $ Person "bob" "saget" 45
   @

   is effectively equivalent to:

   @
   console.log({first: "bob", last: "saget", age: 45})
   @

   in that the console will render with nice expand/collapse object exploration features.
-}
class LogJS (c :: Type -> Constraint) where
  logJS :: c a => Text -> a -> JSM ()


-- | Logs against 'ToJSON' will be encoded via 'Aeson' then parsed using
-- native <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse JSON.parse> before being sent to the console.
instance LogJS ToJSON where
  logJS t a = do
    console <- jsg "console"
    json    <- jsg "JSON"
    parsed  <- json ^. js1 "parse" (toStrict . decodeUtf8 $ encode a)
    () <$ console ^. js1 t parsed


-- | Logs against 'Show' will be converted to a 'String' before being sent to the console.
instance LogJS Show where
  logJS t a = do
    console <- jsg "console"
    () <$ console ^. js1 t (pack $ show a)


-- | Logs against 'ToJSVal' will be converted to a 'JSVal' before being sent to the console.
instance LogJS ToJSVal where
  logJS t a = do
    console <- jsg "console"
    () <$ console ^. js1 t (toJSVal a)


{-|
  Trapper is a class intended for continuous logging of your application and the catching of helpless animals.
  Usage is along the lines of 'Debug.Trace.trace' where the effect of logging is implicit.
  To make this work in both GHC and GHCjs contexts, you do need to
  pass the 'JSContextRef' in manually ('askJSM' re-exported here for convenience).

  @
  main :: IO ()
  main = runJSorWarp 8080 $ do
    ctx <- askJSM
    simple runParDiff initial (view . trapper @ToJSON ctx) getBody
  @
-}
class LogJS c => Trapper c where
  trapper :: c a => JSContextRef -> a -> a
  trapper ctx x = unsafePerformIO $ runJSM (x <$ debug @c x) ctx
  {-# NOINLINE trapper #-}

instance Trapper ToJSON
instance Trapper Show
instance Trapper ToJSVal


{-|
  Assert is a class for assertion programming. It behaves the same as 'LogJS' but calls
  <https://developer.mozilla.org/en-US/docs/Web/API/Console/assert console.assert> instead of
  other console methods. This will only have an effect if the 'Bool' provided to 'assert' is 'False'.
-}
class Assert (c :: Type -> Constraint) where
  assert :: c a => Bool -> a -> JSM ()

instance Assert ToJSON where
  assert b x = do
    console <- jsg "console"
    json <- jsg "JSON"
    parsed <- json ^. js1 "parse" (toStrict . decodeUtf8 $ encode x)
    () <$ console ^. js2 "assert" (toJSVal b) parsed

instance Assert Show where
  assert b x = do
    console <- jsg "console"
    () <$ console ^. js2 "assert" (toJSVal b) (pack $ show x)

instance Assert ToJSVal where
  assert b x = do
    console <- jsg "console"
    () <$ console ^. js2 "assert" (toJSVal b) (toJSVal x)


-- | Log a list of JSON objects to the console where it will rendered as a table using <https://developer.mozilla.org/en-US/docs/Web/API/Console/table console.table>
table :: ToJSON a => [a] -> JSM ()
table = logJS @ToJSON "table"


-- | Log to the console using <https://developer.mozilla.org/en-US/docs/Web/API/Console/log console.log>
log :: forall c a. LogJS c => c a => a -> JSM ()
log = logJS @c "log"


-- | Log with the "warn" log level using <https://developer.mozilla.org/en-US/docs/Web/API/Console/warn console.warn>
warn :: forall c a. LogJS c => c a => a -> JSM ()
warn = logJS @c "warn"


-- | Log with the "info" log level using <https://developer.mozilla.org/en-US/docs/Web/API/Console/info console.info>
info :: forall c a. LogJS c => c a => a -> JSM ()
info = logJS @c "info"


-- | Log with the "debug" log level using <https://developer.mozilla.org/en-US/docs/Web/API/Console/debug console.debug>
debug :: forall c a. LogJS c => c a => a -> JSM ()
debug = logJS @c "debug"


-- | A unique label for a timer. This is used to tie calls to <https://developer.mozilla.org/en-US/docs/Web/API/Console/time console.time> to <https://developer.mozilla.org/en-US/docs/Web/API/Console/timeEnd console.timeEnd>
newtype TimeLabel = TimeLabel { unTimeLabel :: Text }
  deriving (Eq, Ord, Show, IsString)


-- | Start a timer using <https://developer.mozilla.org/en-US/docs/Web/API/Console/time console.time>
time :: TimeLabel -> JSM ()
time (TimeLabel l) = do
  console <- jsg "console"
  () <$ console ^. js1 "time" l


-- | End a timer and print the milliseconds elapsed since it started using <https://developer.mozilla.org/en-US/docs/Web/API/Console/timeEnd console.timeEnd>
timeEnd :: TimeLabel -> JSM ()
timeEnd (TimeLabel l) = do
  console <- jsg "console"
  () <$ console ^. js1 "timeEnd" l
