{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE JavaScriptFFI          #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

module Shpadoinkle.JSFFI
  ( JSM
  , MonadJSM
  , liftJSM

  , ghcjsOnly

  , To
  , PTo
  , to
  , purely

  , jsTreq

  , IsJSVal
  , fromJSValUnsafe

  , JSVal
  , toJSVal

  , JSString
  , toJSString
  , jsStringToString
  , jsStringToText

  , JSObject
  , toJSObject
  , mkEmptyObject
  , getProp
  , getPropCoerce
  , setProp
  , setPropCoerce

  , JSKey
  , toJSKey

  , JSArray
  , toJSArray
  , jsArrayFromList
  , jsArrayToList

  , JSFunction
  , HSFunction
  , toJSFunction
  , mkFun
  , mkFun'
  , (#)
  , (#!)
  , setTimeout
  , clearTimeout

  , JSElement
  , toJSElement
  , setInnerHTML
  , createElement
  , appendChild
  , setId
  , setAttribute
  , getElementById

  , JSBool
  , toJSBool
  , jsTrue
  , jsFalse

  , JSStorage
  , localStorage
  , sessionStorage
  , getItem
  , setItem

  , global
  , window
  , document
  , body
  , setTitle

  , getGlobal
  , JSContextRef
  , askJSM
  , runJSM
  , eval
  , createTextNode

  , jsNull
  , jsUndefined

  , toStringLax
  , asString
  , toTextLax
  , asText
  , toNumberLax
  , toIntLax
  , toBoolLax

  , requestAnimationFrame
  , requestAnimationFrame_

  , getLocation
  , getLocationHref
  , getLocationPathname
  , getLocationSearch

  , onWindowPopstateWithoutEvent
  , scrollTo
  , historyPushState
  ) where


import           Control.Monad             (void, (<=<), (>=>))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Functor.Identity     (Identity (..))
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.IO.Unsafe          (unsafePerformIO)
import           Unsafe.Coerce             (unsafeCoerce)
#ifdef ghcjs_HOST_OS
import           Control.Category          ((<<<), (>>>))
import           Control.Monad.IO.Class    (liftIO)
import           Data.Coerce               (coerce)
import           Data.Traversable          (for)
#endif

-- ghcjs imports
#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign.Callback    (Callback, OnBlocked (ContinueAsync),
                                            syncCallback2)
import qualified GHCJS.Marshal.Internal    as Ghcjs
import           GHCJS.Nullable            (Nullable, maybeToNullable,
                                            nullableToMaybe)
import           GHCJS.Prim                (JSVal)
import qualified GHCJS.Prim                as Ghcjs
import           JavaScript.Array.Internal (SomeJSArray (..), toListIO)
#endif


#ifndef ghcjs_HOST_OS
data JSVal
#endif

type JSM = IO

type MonadJSM = MonadIO

liftJSM :: MonadJSM m => JSM a -> m a
liftJSM = liftIO


ghcjsOnly :: a
ghcjsOnly = error "Attempted to perform an operation which is permitted on GHCjs only. Are you compiling with GHC? The GHC Shpadoinkle API is intended for typechecking only; you must compile to GHCjs in order to actually run anything that requires Javascript. (This is most things, unless you are using the static backend.)"

class To f b a where
  to :: a -> f b

-- WANT^ This class is used by some client code.
--       Hence, it could probably use a rename.

type PTo = To Identity

-- |
--
-- If exists instance To Identity B A, then have both:
-- * to :: A -> Identity B
-- * purely to :: A -> B
purely :: (a -> Identity b) -> (a -> b)
purely = fmap runIdentity

instance To JSM b a => To JSM b (JSM a) where
  to = (>>= to)


toJSVal :: To m JSVal a => a -> m JSVal
toJSVal = to

instance Applicative m => To m JSVal JSVal where
  to = pure

instance Applicative m => To m JSVal () where
  to () = pure jsUndefined

instance Applicative m => To m JSVal Double where
#ifdef ghcjs_HOST_OS
  to = pure . Ghcjs.pToJSVal
#else
  to = ghcjsOnly
#endif

instance Applicative m => To m JSVal Bool where
#ifdef ghcjs_HOST_OS
  to = pure . Ghcjs.pToJSVal
#else
  to = ghcjsOnly
#endif

instance Applicative m => To m JSVal Int where
#ifdef ghcjs_HOST_OS
  to = pure . Ghcjs.pToJSVal
#else
  to = ghcjsOnly
#endif

instance (MonadJSM m, To m JSVal a) => To m JSVal [a] where
  to = traverse to >=> (liftJSM . jsArrayFromList) >=> to

instance To JSM JSVal a => To JSM JSVal (JSM a) where
  to = (>>= to)

instance (Applicative m, To m JSVal a) => To m JSVal (Maybe a) where
  to = \case
    Nothing -> pure jsUndefined
    Just a -> to a

instance {-# OVERLAPPING #-} Applicative m => To m JSVal String where
  to = pure . purely toJSVal . purely toJSString . T.pack

instance Applicative m => To m JSVal Text where
  to = pure . purely toJSVal . purely toJSString


-- triple-equals comparison
jsTreq :: (PTo JSVal a, PTo JSVal b) => a -> b -> Bool
#ifdef ghcjs_HOST_OS
jsTreq a b = jsTreq_js (purely toJSVal a) (purely toJSVal b)

foreign import javascript unsafe
  "$1 === $2"
  jsTreq_js :: JSVal -> JSVal -> Bool
#else
jsTreq = ghcjsOnly
#endif


-- JSVal newtypes
class PTo JSVal a => IsJSVal a where
  fromJSValUnsafe :: JSVal -> a


--------------------------------------------------------------------------------

newtype JSString = JSString { unJSString :: JSVal }

toJSString :: To m JSString a => a -> m JSString
toJSString = to

instance Applicative m => To m JSString JSString where
  to = pure

instance Applicative m => To m JSVal JSString where
  to = pure . unJSString

instance Applicative m => To m JSString String where
#ifdef ghcjs_HOST_OS
  to = pure . JSString . Ghcjs.toJSString
#else
  to = ghcjsOnly
#endif

instance Applicative m => To m JSString Text where
#ifdef ghcjs_HOST_OS
  to = pure . JSString . Ghcjs.toJSString . T.unpack
#else
  to = ghcjsOnly
#endif

jsStringToText :: JSString -> Text
jsStringToText = unsafeCoerce
  -- WANT^ to test

jsStringToString :: JSString -> String
jsStringToString = T.unpack . jsStringToText

instance IsJSVal JSString where
  fromJSValUnsafe = JSString

instance IsString JSString where
  fromString = purely to

instance Semigroup JSString where
#ifndef ghcjs_HOST_OS
  (<>) = ghcjsOnly
#else
  JSString a <> JSString b = JSString (a `addStr_js` b)

foreign import javascript unsafe
  "$1 + $2"
  addStr_js :: JSVal -> JSVal -> JSVal
#endif

instance Monoid JSString where
#ifndef ghcjs_HOST_OS
  mempty = ghcjsOnly
#else
  mempty = JSString emptyStr_js

foreign import javascript unsafe
  "$r = ''"
  emptyStr_js :: JSVal
#endif


--------------------------------------------------------------------------------

newtype JSObject = JSObject { unJSObject :: JSVal }

toJSObject :: To f JSObject a => a -> f JSObject
toJSObject = to

instance Applicative m => To m JSObject JSObject where
  to = pure

instance Applicative m => To m JSVal JSObject where
  to = pure . unJSObject

instance IsJSVal JSObject where
  fromJSValUnsafe = JSObject

mkEmptyObject :: MonadJSM m => m JSObject
#ifdef ghcjs_HOST_OS
mkEmptyObject = JSObject <$> liftJSM mkEmptyObject_js
foreign import javascript unsafe
  "$r = ({})"
  mkEmptyObject_js :: JSM JSVal
#else
mkEmptyObject = ghcjsOnly
#endif

getProp :: (MonadJSM m, To m JSKey key, To m JSObject obj) => key -> obj -> m JSVal
getProp k o = do
  k' <- toJSKey k
  getPropCoerce k' o

-- | Allows any JSVal as key
getPropCoerce :: (MonadJSM m, To m JSVal key, To m JSObject obj) => key -> obj -> m JSVal
#ifdef ghcjs_HOST_OS
getPropCoerce k o = do
  k' <- toJSVal k
  o' <- unJSObject <$> toJSObject o
  liftJSM $ getProp_js k' o'

foreign import javascript unsafe
  "$r = $2[$1]"
  getProp_js :: JSVal -> JSVal -> JSM JSVal
#else
getPropCoerce = ghcjsOnly
#endif


setProp :: (MonadJSM m, To m JSKey key, To m JSVal val, To m JSObject obj) => key -> val -> obj -> m ()
setProp k v o = do
  k' <- toJSKey k
  setPropCoerce k' v o

-- | Allows any JSVal as key
setPropCoerce :: (MonadJSM m, To m JSVal key, To m JSVal val, To m JSObject obj) => key -> val -> obj -> m ()
#ifdef ghcjs_HOST_OS
setPropCoerce k v o = do
  k' <- toJSVal k
  v' <- toJSVal v
  o' <- unJSObject <$> toJSObject o
  liftJSM $ setProp_js k' v' o'

foreign import javascript unsafe
  "$3[$1] = $2"
  setProp_js :: JSVal -> JSVal -> JSVal -> JSM ()
#else
setPropCoerce = ghcjsOnly
#endif


--------------------------------------------------------------------------------

-- string or symbol
newtype JSKey = JSKey { unJSKey :: JSVal }

toJSKey :: To m JSKey a => a -> m JSKey
toJSKey = to

instance IsJSVal JSKey where
  fromJSValUnsafe = JSKey

instance Applicative m => To m JSVal JSKey where
  to = pure . unJSKey

instance Applicative m => To m JSKey JSKey where
  to = pure

instance Applicative m => To m JSKey JSString where
  to = pure . JSKey . purely toJSVal

instance Applicative m => To m JSKey String where
  to = pure . JSKey . purely toJSVal . purely toJSString . T.pack

instance Applicative m => To m JSKey Text where
  to = pure . JSKey . purely toJSVal . purely toJSString

instance Applicative m => To m JSKey Int where
  to = to . show


--------------------------------------------------------------------------------

newtype JSArray = JSArray { unJSArray :: JSVal }

toJSArray :: To m JSArray a => a -> m JSArray
toJSArray = to

instance IsJSVal JSArray where
  fromJSValUnsafe = JSArray

instance Applicative m => To m JSVal JSArray where
  to = pure . unJSArray

instance Applicative m => To m JSObject JSArray where
  to = pure . JSObject . unJSArray

instance Applicative m => To m JSArray JSArray where
  to = pure

jsArrayFromList :: [JSVal] -> JSM JSArray
#ifdef ghcjs_HOST_OS
jsArrayFromList xs = do
  ar <- newArray_js (purely toJSVal $ length xs)
  let ar' = JSObject ar
  for (zip [0..] xs) $ \(idx, val) -> setProp (idx :: Int) val ar'
  pure $ JSArray ar

foreign import javascript unsafe
  "$r = Array.from(Array($1))"
      -- nb. "pre-allocate" for possible perf boost. unbenchmarked.
  newArray_js :: JSVal -> JSM JSVal
#else
jsArrayFromList = ghcjsOnly
#endif

jsArrayToList :: JSArray -> JSM [JSVal]
#ifdef ghcjs_HOST_OS
jsArrayToList (JSArray ar) = do
  let ar' = JSObject ar
  len <- toIntLax =<< getProp "length" ar'
  for [0 :: Int .. len - 1] $ \idx -> getProp idx ar'
#else
jsArrayToList = ghcjsOnly
#endif


--------------------------------------------------------------------------------

newtype JSFunction = JSFunction { unJSFunction :: JSVal }

toJSFunction :: To m JSFunction a => a -> m JSFunction
toJSFunction = to

instance IsJSVal JSFunction where
  fromJSValUnsafe = JSFunction

instance Applicative m => To m JSVal JSFunction where
  to = pure . unJSFunction

instance Applicative m => To m JSFunction JSFunction where
  to = pure

instance Applicative m => To m JSObject JSFunction where
  to = pure . JSObject . unJSFunction


-- Function type for Haskell/JS interpolation
type HSFunction =
     JSVal   -- ^ function object
  -> JSVal   -- ^ this
  -> [JSVal] -- ^ function args
  -> JSM ()

-- | Make a JSFunction from a Haskell function (of compatible type)
mkFun :: MonadJSM m => HSFunction -> m JSFunction
#ifdef ghcjs_HOST_OS
mkFun fun =
  let
    middleman :: JSVal -> JSVal -> IO ()
    middleman this args = do
      argsList <- toListIO (coerce args)
      fun this this argsList
        -- FIXME
        -- We are passing 'this' for the function object, which is wrong
        -- This bug is inherited from JSaddle and may be relied upon by client code

  in liftJSM $ do
       cb <- syncCallback2 ContinueAsync middleman
       JSFunction <$> callbackToJSVal cb

foreign import javascript unsafe
  "$r = function() { $1(this, arguments); }"
  callbackToJSVal :: Callback (JSVal -> JSVal -> IO ()) -> IO JSVal
#else
mkFun = ghcjsOnly
#endif

-- | Like @mkFun@ but for functions which don't use the self-references
mkFun' :: MonadJSM m => ([JSVal] -> JSM ()) -> m JSFunction
mkFun' fun = mkFun (\_fun _this args -> fun args)


setTimeout :: MonadJSM m => Int -> JSFunction -> m Int
#ifdef ghcjs_HOST_OS
setTimeout delay (JSFunction fun) =
    liftJSM $ toIntLax =<< setTimeout_js fun (purely toJSVal delay)

foreign import javascript unsafe
  "$r = globalThis.setTimeout($1, $2)"
  setTimeout_js :: JSVal -> JSVal -> JSM JSVal
#else
setTimeout = ghcjsOnly
#endif


clearTimeout :: MonadJSM m => Int -> m ()
#ifdef ghcjs_HOST_OS
clearTimeout tid =
    liftJSM $ clearTimeout_js (purely toJSVal tid)

foreign import javascript unsafe
  "globalThis.clearTimeout($1)"
  clearTimeout_js :: JSVal -> JSM ()
#else
clearTimeout = ghcjsOnly
#endif


newtype JSArgs = JSArgs { unJSArgs :: JSArray }

toJSArgs :: To m JSArgs a => a -> m JSArgs
toJSArgs = to

instance Applicative m => To m JSArgs JSArgs where
  to = pure

instance MonadJSM m => To m JSArgs JSVal where
  to = liftJSM . fmap JSArgs . jsArrayFromList . (:[])

instance MonadJSM m => To m JSArgs () where
  to = toJSVal >=> toJSArgs

instance MonadJSM m => To m JSArgs JSString where
  to = toJSVal >=> toJSArgs

instance MonadJSM m => To m JSArgs String where
  to = toJSVal >=> toJSArgs

instance MonadJSM m => To m JSArgs Text where
  to = toJSVal >=> toJSArgs

instance MonadJSM m => To m JSArgs JSFunction where
  to = toJSVal >=> toJSArgs

instance (MonadJSM m, To m JSVal a) => To m JSArgs [a] where
  to = liftJSM . fmap JSArgs . jsArrayFromList <=< traverse toJSVal

instance (MonadJSM m, To m JSVal a1, To m JSVal a2) => To m JSArgs (a1, a2) where
  to (a1, a2) = do
    a1' <- toJSVal a1
    a2' <- toJSVal a2
    toJSArgs [a1', a2']

instance (MonadJSM m, To m JSVal a1, To m JSVal a2, To m JSVal a3) => To m JSArgs (a1, a2, a3) where
  to (a1, a2, a3) = do
    a1' <- toJSVal a1
    a2' <- toJSVal a2
    a3' <- toJSVal a3
    toJSArgs [a1', a2', a3']


-- | Call a JS method, safe(r) edition
(#) :: (MonadJSM m, To m JSObject this, To m JSKey prop, To m JSArgs args) => this -> prop -> args -> m JSVal
(#) this prop args = do
  prop' <- toJSKey prop
  (#!) this prop' args
infixr 2 #

-- | Call a JS method, unsafe edition
(#!) :: (MonadJSM m, To m JSObject this, To m JSVal prop, To m JSArgs args) => this -> prop -> args -> m JSVal
#ifdef ghcjs_HOST_OS
(#!) this prop args = do
  this' <- unJSObject <$> toJSObject this
  prop' <- toJSVal prop
  args' <- unJSArray . unJSArgs <$> toJSArgs args
  liftJSM $ unsafeCall_js this' prop' args'

foreign import javascript unsafe
  "$r = (function (it) { return it[$2].apply(it, $3); })($1)"
  unsafeCall_js :: JSVal -> JSVal -> JSVal -> JSM JSVal
#else
(#!) = ghcjsOnly
#endif
infixr 2 #!


--------------------------------------------------------------------------------

newtype JSElement = JSElement { unJSElement :: JSVal }

toJSElement :: To m JSElement a => a -> m JSElement
toJSElement = to

instance Applicative m => To m JSVal JSElement where
  to = pure . unJSElement

instance IsJSVal JSElement where
  fromJSValUnsafe = JSElement

setInnerHTML :: (MonadJSM m, To m JSString s) => s -> JSElement -> m ()
#ifdef ghcjs_HOST_OS
setInnerHTML str el = do
  str' <- toJSVal =<< toJSString str
  liftJSM $ setInnerHTML_js (unJSElement el) str'

foreign import javascript unsafe
  "$1.innerHTML = $2"
  setInnerHTML_js :: JSVal -> JSVal -> JSM ()
#else
setInnerHTML = ghcjsOnly
#endif

createElement :: MonadJSM m => Text -> m JSElement
#ifdef ghcjs_HOST_OS
createElement name =
  toJSVal name >>= (fmap JSElement . liftJSM <$> createElement_js)

foreign import javascript unsafe
  "$r = document.createElement($1)"
  createElement_js :: JSVal -> JSM JSVal
#else
createElement = ghcjsOnly
#endif

appendChild :: (MonadJSM m, To m JSVal ch) => ch -> JSElement -> m ()
#ifdef ghcjs_HOST_OS
appendChild child parent = do
  child' <- toJSVal child
  liftJSM $ appendChild_js (unJSElement parent) child'

foreign import javascript unsafe
  "$1.appendChild($2)"
  appendChild_js :: JSVal -> JSVal -> JSM ()
#else
appendChild = ghcjsOnly
#endif

setId :: MonadJSM m => Text -> JSElement -> m ()
#ifdef ghcjs_HOST_OS
setId newId el =
  liftJSM $ setId_js (unJSElement el) (purely toJSVal newId)

foreign import javascript unsafe
  "$1.id = $2"
  setId_js :: JSVal -> JSVal -> JSM ()
#else
setId = ghcjsOnly
#endif

setAttribute :: MonadJSM m => Text -> Text -> JSElement -> m ()
#ifdef ghcjs_HOST_OS
setAttribute attr val el =
  liftJSM $ setAttribute_js (unJSElement el) (purely toJSVal attr) (purely toJSVal val)

foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  setAttribute_js :: JSVal -> JSVal -> JSVal -> JSM ()
#else
setAttribute = ghcjsOnly
#endif

getElementById :: MonadJSM m => Text -> m (Maybe JSElement)
#ifdef ghcjs_HOST_OS
getElementById eid = do
  el <- liftJSM $ getElementById_js (purely toJSVal eid)
  pure $ JSElement <$> nullableToMaybe el

foreign import javascript unsafe
  "$r = document.getElementById($1)"
  getElementById_js :: JSVal -> JSM (Nullable JSVal)
#else
getElementById = ghcjsOnly
#endif


--------------------------------------------------------------------------------

newtype JSBool = JSBool { unJSBool :: JSVal }

toJSBool :: To m JSBool a => a -> m JSBool
toJSBool = to

instance IsJSVal JSBool where
  fromJSValUnsafe = JSBool

instance Applicative m => To m JSVal JSBool where
  to = pure . unJSBool

instance Applicative m => To m JSBool JSBool where
  to = pure

instance Applicative m => To m JSBool Bool where
  to = \case
    True -> pure jsTrue
    False -> pure jsFalse

jsTrue, jsFalse :: JSBool
#ifdef ghcjs_HOST_OS
jsTrue = JSBool $ unsafePerformIO jsTrue_js
jsFalse = JSBool $ unsafePerformIO jsFalse_js
foreign import javascript unsafe "$r = true" jsTrue_js :: JSM JSVal
foreign import javascript unsafe "$r = false" jsFalse_js :: JSM JSVal
#else
jsTrue = ghcjsOnly
jsFalse = ghcjsOnly
#endif


--------------------------------------------------------------------------------

newtype JSStorage = JSStorage { unJSStorage :: JSVal }

toJSStorage :: To m JSStorage a => a -> m JSStorage
toJSStorage = to

instance IsJSVal JSStorage where
  fromJSValUnsafe = JSStorage

instance Applicative m => To m JSStorage JSStorage where
  to = pure

instance Applicative m => To m JSVal JSStorage where
  to = pure . unJSStorage

localStorage, sessionStorage :: JSStorage
#ifdef ghcjs_HOST_OS
localStorage = JSStorage $ unsafePerformIO (getProp "localStorage" window)
sessionStorage = JSStorage $ unsafePerformIO (getProp "sessionStorage" window)
#else
localStorage = ghcjsOnly
sessionStorage = ghcjsOnly
#endif

setItem :: (MonadJSM m, To m JSString key, To m JSString val) => key -> val -> JSStorage -> m ()
#ifdef ghcjs_HOST_OS
setItem key val store = do
  key' <- toJSVal =<< toJSString key
  val' <- toJSVal =<< toJSString val
  liftJSM $ setItem_js (unJSStorage store) key' val'

foreign import javascript unsafe
  "$1.setItem($2, $3)"
  setItem_js :: JSVal -> JSVal -> JSVal -> JSM ()
#else
setItem = ghcjsOnly
#endif

getItem :: (MonadJSM m, To m JSString key) => key -> JSStorage -> m (Maybe JSString)
#ifdef ghcjs_HOST_OS
getItem key store = do
  key' <- toJSVal =<< toJSString key
  mStr <- liftJSM $ nullableToMaybe <$> getItem_js (unJSStorage store) key'
  pure $ JSString <$> mStr

foreign import javascript unsafe
  "$r = $1.getItem($2)"
  getItem_js :: JSVal -> JSVal -> JSM (Nullable JSVal)
#else
getItem = ghcjsOnly
#endif


-------------------------------------------------------------------------------


global :: JSObject
#ifdef ghcjs_HOST_OS
global = fromJSValUnsafe $ unsafePerformIO global_js
foreign import javascript unsafe
  "$r = globalThis"
  global_js :: JSM JSVal
#else
global = ghcjsOnly
#endif

window :: JSObject
#ifdef ghcjs_HOST_OS
window = fromJSValUnsafe $ unsafePerformIO window_js
foreign import javascript unsafe
  "$r = window"
  window_js :: JSM JSVal
#else
window = ghcjsOnly
#endif

document :: JSObject
#ifdef ghcjs_HOST_OS
document = fromJSValUnsafe $ unsafePerformIO document_js
foreign import javascript unsafe
  "$r = document"
  document_js :: JSM JSVal
#else
document = ghcjsOnly
#endif

body :: JSElement
#ifdef ghcjs_HOST_OS
body = fromJSValUnsafe $ unsafePerformIO body_js
foreign import javascript unsafe
  "$r = document.body"
  body_js :: JSM JSVal
#else
body = ghcjsOnly
#endif


setTitle :: forall m s. (MonadJSM m, To m JSString s) => s -> m ()
#ifdef ghcjs_HOST_OS
setTitle title = do
  title' :: JSString <- to title
  title'' :: JSVal <- to title'
  liftJSM $ setTitle_js title''

foreign import javascript unsafe
  "document.title = $1"
  setTitle_js :: JSVal -> JSM ()
#else
setTitle = ghcjsOnly
#endif


-- WANT: replace callsites with 'global' and 'getProp'/'(#)'
getGlobal :: (MonadJSM m, To m JSKey k) => k -> m JSVal
#ifdef ghcjs_HOST_OS
getGlobal = (liftJSM <$> getGlobal_js) <=< toJSVal <=< toJSKey
foreign import javascript unsafe
  "$r = globalThis[$1]"
  getGlobal_js :: JSVal -> JSM JSVal
#else
getGlobal = ghcjsOnly
#endif


-- | Convert to string by calling '.toString()'
toTextLax :: MonadJSM m => JSVal -> m Text
#ifdef ghcjs_HOST_OS
toTextLax = liftJSM . toTextLax_js
foreign import javascript unsafe
  "$r = $1.toString()"
  toTextLax_js :: JSVal -> JSM Text
#else
toTextLax = ghcjsOnly
#endif

asText :: MonadJSM m => JSVal -> m (Maybe Text)
#ifdef ghcjs_HOST_OS
asText = liftJSM . fmap nullableToMaybe . asText_js
foreign import javascript unsafe
  "$r = typeof $1 === 'string' ? $1 : null"
  asText_js :: JSVal -> JSM (Nullable Text)
#else
asText = ghcjsOnly
#endif

toStringLax :: MonadJSM m => JSVal -> m String
toStringLax = fmap T.unpack . toTextLax

asString :: MonadJSM m => JSVal -> m (Maybe String)
asString = (fmap . fmap) T.unpack . asText

-- | Convert to a number by passing to 'Number()'
toNumberLax :: MonadJSM m => JSVal -> m Double
#ifdef ghcjs_HOST_OS
toNumberLax = liftJSM . toNumberLax_js
foreign import javascript unsafe
  "Number($1)"
  toNumberLax_js :: JSVal -> JSM Double
#else
toNumberLax = ghcjsOnly
#endif

-- | 'toNumberLax' followed by 'round'
toIntLax :: JSVal -> JSM Int
toIntLax = fmap round . toNumberLax

-- | Is the value truthy?
toBoolLax :: JSVal -> JSM Bool
#ifdef ghcjs_HOST_OS
toBoolLax = toBoolLax_js
foreign import javascript unsafe
  "!(!$1)"
  toBoolLax_js :: JSVal -> JSM Bool
#else
toBoolLax = ghcjsOnly
#endif


jsNull :: JSVal
#ifdef ghcjs_HOST_OS
jsNull = unsafePerformIO jsNull_js
foreign import javascript unsafe
  "$r = null"
  jsNull_js :: JSM JSVal
#else
jsNull = ghcjsOnly
#endif


jsUndefined :: JSVal
#ifdef ghcjs_HOST_OS
jsUndefined = unsafePerformIO jsUndefined_js
foreign import javascript unsafe
  "$r = undefined"
  jsUndefined_js :: JSM JSVal
#else
jsUndefined = ghcjsOnly
#endif


-- This all is trivial and is included only because S11 re-exports it
-- WANT: remove it from S11 and then from here
type JSContextRef = ()
askJSM :: MonadJSM m => m JSContextRef
askJSM = pure ()
runJSM :: MonadIO m => JSM a -> JSContextRef -> m a
#ifdef ghcjs_HOST_OS
runJSM act _ = liftIO act
#else
runJSM = ghcjsOnly
#endif

eval :: forall s m. (MonadJSM m, To m JSString s) => s -> m JSVal
#ifdef ghcjs_HOST_OS
eval = toJSString >=> (liftJSM <$> eval_js)
foreign import javascript unsafe
  "$r = eval($1)"
  eval_js :: JSString -> IO JSVal
#else
eval = ghcjsOnly
#endif


createTextNode :: MonadJSM m => Text -> m JSVal
#ifdef ghcjs_HOST_OS
createTextNode = liftJSM . createTextNode_js

foreign import javascript unsafe
  "$r = document.createTextNode($1)"
  createTextNode_js :: Text -> JSM JSVal
#else
createTextNode = ghcjsOnly
#endif


requestAnimationFrame :: (Double -> JSM ()) -> JSM Int
requestAnimationFrame cb = do
  fun <- mkFun' $ \case
    [t] -> toNumberLax t >>= cb
    _ -> pure ()
  ret <- window # "requestAnimationFrame" $ fun
  round <$> toNumberLax ret

requestAnimationFrame_ :: (Double -> JSM ()) -> JSM ()
requestAnimationFrame_ = void . requestAnimationFrame


getLocation :: JSM JSObject
getLocation = fromJSValUnsafe @JSObject <$> getProp "location" window

getLocationHref, getLocationPathname, getLocationSearch :: JSM Text
getLocationHref     = toTextLax =<< getProp prop =<< getLocation where prop = "href"
getLocationPathname = toTextLax =<< getProp prop =<< getLocation where prop = "pathname"
getLocationSearch   = toTextLax =<< getProp prop =<< getLocation where prop = "search"

-- yeah i write generic code
onWindowPopstateWithoutEvent :: JSM () -> JSM ()
#ifdef ghcjs_HOST_OS
onWindowPopstateWithoutEvent cb = do
  cb' <- mkFun' (\_ -> cb)
  void $ window # "addEventListener" $ ("popstate", cb')
#else
onWindowPopstateWithoutEvent = ghcjsOnly
#endif

scrollTo :: MonadJSM m => Double -> Double -> m ()
scrollTo x y = void $ window # "scrollTo" $ (x, y)

historyPushState
  :: (MonadJSM m, To m JSVal data_, To m JSVal title, To m JSVal url)
  => data_ -> title -> Maybe url -> m ()
historyPushState data_ title mUrl = do
  history <- liftJSM $ fromJSValUnsafe @JSObject <$> getProp "history" window
  args <- toJSArgs (data_, title, mUrl)
  void . liftJSM $ history # "pushState" $ args
