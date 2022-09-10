{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}

module Shpadoinkle.JSFFI
  ( module Shpadoinkle.JSFFI
  , JSM
  , MonadJSM
  , liftJSM
  , JSVal
  , JSString
  , Window
  ) where

import           Control.Monad.IO.Class      (MonadIO)
import Control.Monad ((<=<))
import           Unsafe.Coerce               (unsafeCoerce)
import           System.IO.Unsafe            (unsafePerformIO)
#ifdef ghcjs_HOST_OS
import           Control.Category            ((<<<), (>>>))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coerce                 (coerce)
import Data.Traversable (for)
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
import           Language.Javascript.JSaddle (JSM, MonadJSM, PToJSVal,
                                              ToJSString, ToJSVal, liftJSM,
                                              pToJSVal, toJSString, toJSVal)


ghcjsOnly :: a
ghcjsOnly = error "Attempted to perform an operation which is permitted on GHCjs only. Are you compiling with GHC? The GHC Shpadoinkle API is intended for typechecking only; you must compile to GHCjs in order to actually run anything that requires Javascript. (This is most things, unless you are using the static backend.)"


--------------------------------------------------------------------------------
-- JSObject

newtype JSObject = JSObject { unJSObject :: JSVal }

mkEmptyObject :: MonadJSM m => m JSObject
#ifdef ghcjs_HOST_OS
mkEmptyObject = JSObject <$> liftJSM mkEmptyObject_js
foreign import javascript unsafe
  "$r = ({})"
  mkEmptyObject_js :: JSM JSVal
#else
mkEmptyObject = ghcjsOnly
#endif

-- string or symbol
newtype JSKey = JSKey { unJSKey :: JSVal }

class ToJSKey key where
  toJSKey :: key -> JSM JSKey

instance ToJSKey JSKey where
  toJSKey = pure

instance ToJSKey String where
  toJSKey = fmap JSKey . toJSVal

instance ToJSKey Int where
  toJSKey = fmap JSKey . toJSVal . show


getProp :: (ToJSKey key, ToJSObject obj) => key -> obj -> JSM JSVal
getProp k o = toJSKey k >>= \(JSKey k') -> getPropCoerce k' o

-- | Allows any JSVal as key
getPropCoerce :: (ToJSVal key, ToJSObject obj) => key -> obj -> JSM JSVal
#ifdef ghcjs_HOST_OS
getPropCoerce k o = undefined
#else
getPropCoerce = ghcjsOnly
#endif


setProp :: (ToJSKey key, ToJSVal val, ToJSObject obj) => key -> val -> obj -> JSM ()
setProp k v o = toJSKey k >>= \(JSKey k') -> setPropCoerce k' v o

-- | Allows any JSVal as key
setPropCoerce :: (ToJSVal key, ToJSVal val, ToJSObject obj) => key -> val -> obj -> JSM ()
#ifdef ghcjs_HOST_OS
setPropCoerce k v o = do
  k' <- toJSVal k
  v' <- toJSVal v
  o' <- unJSObject <$> toJSObject o
  setProp_js k' v' o'

foreign import javascript unsafe
  "$1[$2] = $3" 
  setProp_js :: JSVal -> JSVal -> JSVal -> JSM ()
#else
setPropCoerce = ghcjsOnly
#endif


class ToJSObject o where
  toJSObject :: o -> JSM JSObject

instance ToJSObject JSObject where toJSObject = pure


--------------------------------------------------------------------------------
-- JSArray

newtype JSArray = JSArray { unJSArray :: JSVal }

instance ToJSObject JSArray where
  toJSObject = pure . JSObject . unJSArray

jsArrayFromList :: [JSVal] -> JSM JSArray
#ifdef ghcjs_HOST_OS
jsArrayFromList xs = do
  ar <- newArray_js (intToJSVal $ length xs)
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
  len <- jsValToInt <$> getProp "length" ar'
  for [0 .. len - 1] $ \idx -> getProp (idx :: Int) ar'
#else
jsArrayToList = ghcjsOnly
#endif


--------------------------------------------------------------------------------
-- JSFunction

newtype JSFunction = JSFunction { unJSFunction :: JSVal }

instance PToJSVal JSFunction where pToJSVal = unJSFunction
instance ToJSVal JSFunction where toJSVal = pure . unJSFunction

instance ToJSObject JSFunction where toJSObject = pure . JSObject . unJSFunction


-- Function type for Haskell/JS interpolation
type HSFunction =
     JSVal   -- ^ function object
  -> JSVal   -- ^ this
  -> [JSVal] -- ^ function args
  -> JSM ()

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


class ToJSArgs args where
  toJSArgs :: args -> JSM JSArray

instance ToJSArgs JSVal where
  toJSArgs = jsArrayFromList . (:[])

instance ToJSVal a => ToJSArgs [a] where
  toJSArgs = jsArrayFromList <=< traverse toJSVal

instance (ToJSVal a1, ToJSVal a2) => ToJSArgs (a1, a2) where
  toJSArgs (a1, a2) = do
    a1' <- toJSVal a1
    a2' <- toJSVal a2
    toJSArgs [a1', a2']


-- | Call a JS function, safe edition
-- WANT: a ToProp would be most appropriate since properties can be string|number|symbol
(#) :: (ToJSObject this, ToJSString prop, ToJSArgs args) => this -> prop -> args -> JSM JSVal
(#) = (#!)
infixr 2 #

-- | Call JS function, unsafe edition
(#!) :: (ToJSObject this, ToJSVal prop, ToJSArgs args) => this -> prop -> args -> JSM JSVal
#ifdef ghcjs_HOST_OS
(#!) this prop args = do
  this' <- unJSObject <$> toJSObject this
  prop' <- toJSVal prop
  args' <- unJSArray <$> toJSArgs args
  unsafeCall_js this' prop' args'

foreign import javascript unsafe
  "$r = (function (it) { return it[$2].apply(it, $3); })($1)"
  unsafeCall_js :: JSVal -> JSVal -> JSVal -> JSM JSVal
#else
(#!) = ghcjsOnly
#endif
infixr 2 #!


--------------------------------------------------------------------------------
-- JSBool

newtype JSBool = JSBool { unJSBool :: JSVal }

instance PToJSVal JSBool where pToJSVal = unJSBool
instance ToJSVal JSBool where toJSVal = pure . unJSBool

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


getGlobal :: ToJSString s => s -> JSM JSVal
#ifdef ghcjs_HOST_OS
getGlobal k = getGlobal_js =<< toJSVal k
foreign import javascript unsafe
  "$r = globalThis[$1]"
  getGlobal_js :: JSVal -> JSM JSVal
#else
getGlobal = ghcjsOnly
#endif


-- nb. Unused at time of writing; remove?
currentWindowUnsafe :: MonadJSM m => m Window
currentWindowUnsafe = liftJSM $ unsafeCoerce <$> getGlobal "window"


#ifdef ghcjs_HOST_OS
-- nb I don't know if these are correct
intToJSVal :: Int -> JSVal
intToJSVal = fromIntegral >>> (unsafeCoerce :: Double -> JSVal)
jsValToInt :: JSVal -> Int
jsValToInt = (unsafeCoerce :: JSVal -> Double) >>> round
  -- 'round' seems fishy, but that's what JSaddle uses ...
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

eval :: (ToJSString s, MonadJSM m) => s -> m JSVal
#ifdef ghcjs_HOST_OS
eval = liftJSM . eval_js . toJSString
foreign import javascript unsafe
  "$r = eval($1)"
  eval_js :: JSString -> IO JSVal
#else
eval = ghcjsOnly
#endif
