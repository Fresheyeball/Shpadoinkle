{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}

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
#ifdef ghcjs_HOST_OS
import           Control.Category            ((<<<), (>>>))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coerce                 (coerce)
import           Unsafe.Coerce               (unsafeCoerce)
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


ghcjsOnly :: a
ghcjsOnly = error "Attempted to perform an operation which is permitted on GHCjs only. Are you compiling with GHC? The GHC Shpadoinkle API is intended for typechecking only; you must compile to GHCjs in order to actually run anything that requires Javascript. (This is most things, unless you are using the static backend.)"


-- nb. Unused at time of writing; remove?
currentWindowUnsafe :: MonadJSM m => m Window
#ifdef ghcjs_HOST_OS
currentWindowUnsafe = liftJSM $ unsafeCoerce <$> currentWindowUnsafe_js
foreign import javascript unsafe
  "$r = globalThis.window"
  currentWindowUnsafe_js :: IO JSVal
#else
currentWindowUnsafe = ghcjsOnly
#endif


newtype JSFunction = JSFunction JSVal

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


#ifdef ghcjs_HOST_OS
-- nb I don't know if these are correct
intToJSVal :: Int -> JSVal
intToJSVal = fromIntegral >>> (unsafeCoerce :: Double -> JSVal)
jsValToInt :: JSVal -> Int
jsValToInt = (unsafeCoerce :: JSVal -> Double) >>> round
  -- 'round' seems fishy, but that's what JSaddle uses ...
#endif


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

