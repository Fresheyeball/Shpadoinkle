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
  , jsStringToJSVal

  , JSString
  , toJSString

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
  , toJSFunction
  , HSFunction
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

  , JSBool
  , toJSBool
  , jsTrue
  , jsFalse

  , global
  , window
  , document
  , body

  , getGlobal
  , JSContextRef
  , askJSM
  , runJSM
  , eval
  , jsValToMaybeText
  , jsValToMaybeString
  , callToString
  , callNumber
  , isTruthy
  , intToJSVal
  , jsValToInt
  ) where


import           Control.Monad               ((<=<), (>=>))
import           Control.Monad.IO.Class      (MonadIO)
import           Data.Functor.Identity       (Identity (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.IO.Unsafe            (unsafePerformIO)
import           Unsafe.Coerce               (unsafeCoerce)
#ifdef ghcjs_HOST_OS
import           Control.Category            ((<<<), (>>>))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coerce                 (coerce)
import           Data.Traversable            (for)
#endif

-- ghcjs imports
import           GHCJS.Types                 (JSString, JSVal)
#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign.Callback      (Callback,
                                              OnBlocked (ContinueAsync),
                                              syncCallback2)
import           JavaScript.Array.Internal   (SomeJSArray (..), toListIO)
#endif

-- Imports from packages which transitively depend on jsaddle
--
-- Since the purpose of this package is to replace JSaddle, so it can be removed
-- from Shpadoinkle, these imports will eventually be removed and replaced with
-- custom counterparts. However, while we are transitioning Shpadoinkle from JSaddle
-- to this package, it helps for the two to share types.
import           GHCJS.DOM.Window            (Window)
import           Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)
import qualified Language.Javascript.JSaddle as JSaddle


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

instance Applicative m => To m JSVal JSVal where
  to = pure

instance {-# INCOHERENT #-} (JSaddle.ToJSVal a, MonadJSM m) => To m JSVal a where
  to = liftJSM . JSaddle.toJSVal

instance Applicative m => To m JSVal JSString where
  to = unsafeCoerce

-- Cannot use since overlaps with previous =(
--instance {-# INCOHERENT #-} (JSaddle.PToJSVal a, Applicative m) => To m JSVal a where
--  to = pure . JSaddle.pToJSVal

toJSVal :: To m JSVal a => a -> m JSVal
toJSVal = to

-- WANTv remove after jsaddle is gone
jsStringToJSVal :: JSString -> JSVal
jsStringToJSVal = unsafeCoerce

instance Applicative m => To m JSVal String where
  to = pure . purely toJSVal . purely toJSString . T.pack

instance Applicative m => To m JSVal Text where
  to = pure . purely toJSVal . purely toJSString

instance {-# INCOHERENT #-} (JSaddle.ToJSString a, Applicative m) => To m JSString a where
  to = pure . JSaddle.toJSString

toJSString :: To m JSString a => a -> m JSString
toJSString = to


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
-- JSObject

type JSObject = JSaddle.Object

-- fake a newtype while still backed by JSaddle Object
_JSObject :: JSVal -> JSObject
_JSObject = unsafeCoerce
unJSObject :: JSObject -> JSVal
unJSObject = unsafeCoerce

toJSObject :: To f JSObject a => a -> f JSObject
toJSObject = to

instance Applicative m => To m JSObject JSObject where
  to = pure

instance Applicative m => To m JSVal JSObject where
  to = pure . unJSObject

instance IsJSVal JSObject where
  fromJSValUnsafe = _JSObject

mkEmptyObject :: MonadJSM m => m JSObject
#ifdef ghcjs_HOST_OS
mkEmptyObject = _JSObject <$> liftJSM mkEmptyObject_js
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
-- JSKey

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
-- JSArray

newtype JSArray = JSArray { unJSArray :: JSVal }

toJSArray :: To m JSArray a => a -> m JSArray
toJSArray = to

instance IsJSVal JSArray where
  fromJSValUnsafe = JSArray

instance Applicative m => To m JSVal JSArray where
  to = pure . unJSArray

instance Applicative m => To m JSObject JSArray where
  to = pure . _JSObject . unJSArray

instance Applicative m => To m JSArray JSArray where
  to = pure

jsArrayFromList :: [JSVal] -> JSM JSArray
#ifdef ghcjs_HOST_OS
jsArrayFromList xs = do
  ar <- newArray_js (intToJSVal $ length xs)
  let ar' = _JSObject ar
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
  let ar' = _JSObject ar
  len <- toInt =<< getProp "length" ar'
  for [0 :: Int .. len - 1] $ \idx -> getProp idx ar'

  where
  toInt = fmap round . callNumber
    -- nb^ jsValToInt seems to fail sometimes? Possibly when used in TemplateHaskell
#else
jsArrayToList = ghcjsOnly
#endif


--------------------------------------------------------------------------------
-- JSFunction

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
  to = pure . _JSObject . unJSFunction


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
    liftJSM $ jsValToInt <$> setTimeout_js fun (intToJSVal delay)

foreign import javascript unsafe
  "$r = globalThis.setTimeout($1, $2)"
  setTimeout_js :: JSVal -> JSVal -> JSM JSVal
#else
setTimeout = ghcjsOnly
#endif


clearTimeout :: MonadJSM m => Int -> m ()
#ifdef ghcjs_HOST_OS
clearTimeout tid =
    liftJSM $ clearTimeout_js (intToJSVal tid)

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
-- JSElement

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
createElement name = toJSVal name >>= (fmap JSElement . liftJSM <$> createElement_js)

foreign import javascript unsafe
  "$r = document.createElement($1)"
  createElement_js :: JSVal -> JSM JSVal
#else
createElement = ghcjsOnly
#endif

appendChild :: MonadJSM m => JSElement -> JSElement -> m ()
#ifdef ghcjs_HOST_OS
appendChild child parent = liftJSM $ appendChild_js (unJSElement parent) (unJSElement child)

foreign import javascript unsafe
  "$1.appendChild($2)"
  appendChild_js :: JSVal -> JSVal -> JSM ()
#else
appendChild = ghcjsOnly
#endif

setId :: MonadJSM m => Text -> JSElement -> m ()
#ifdef ghcjs_HOST_OS
setId newId el = liftJSM $ setId_js (unJSElement el) (purely toJSVal newId)

foreign import javascript unsafe
  "$1.id = $2"
  setId_js :: JSVal -> JSVal -> JSM ()
#else
setId = ghcjsOnly
#endif


--------------------------------------------------------------------------------
-- JSBool

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


-- | nb DON'T USE THIS! it makes things break. To be investigated.
intToJSVal :: Int -> JSVal
#ifdef ghcjs_HOST_OS
intToJSVal = fromIntegral >>> (unsafeCoerce :: Double -> JSVal)
#else
intToJSVal = ghcjsOnly
#endif

-- | nb DON'T USE THIS! it makes things break. To be investigated.
jsValToInt :: JSVal -> Int
#ifdef ghcjs_HOST_OS
jsValToInt = (unsafeCoerce :: JSVal -> Double) >>> round
  -- 'round' seems fishy, but that's what JSaddle uses ...
#else
jsValToInt = ghcjsOnly
#endif


-- JavaScript string is Haskell Data.Text
jsValToMaybeText :: JSVal -> Maybe Text
#ifdef ghcjs_HOST_OS
jsValToMaybeText text =
  let nullable = jsValToMaybeText_js text
      isNull = nullable `jsTreq` jsNull
  in if isNull then Nothing else Just (unsafeCoerce nullable)

foreign import javascript unsafe
  "typeof $1 === 'string' ? $1 : null"
  jsValToMaybeText_js :: JSVal -> JSVal
#else
jsValToMaybeText = ghcjsOnly
#endif


jsValToMaybeString :: JSVal -> Maybe String
jsValToMaybeString = fmap T.unpack . jsValToMaybeText


-- | Convert to string by calling '.toString()'
callToString :: JSVal -> JSM Text
#ifdef ghcjs_HOST_OS
callToString = callToString_js
foreign import javascript unsafe
  "$r = $1.toString()"
  callToString_js :: JSVal -> JSM Text
#else
callToString = ghcjsOnly
#endif


-- | Convert to a number by passig to Number()
callNumber :: JSVal -> JSM Double
#ifdef ghcjs_HOST_OS
callNumber = callNumber_js
foreign import javascript unsafe
  "Number($1)"
  callNumber_js :: JSVal -> JSM Double
#else
callNumber = ghcjsOnly
#endif


isTruthy :: JSVal -> JSM Bool
#ifdef ghcjs_HOST_OS
isTruthy = isTruthy_js
foreign import javascript unsafe
  "!(!$1)"
  isTruthy_js :: JSVal -> JSM Bool
#else
isTruthy = ghcjsOnly
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
