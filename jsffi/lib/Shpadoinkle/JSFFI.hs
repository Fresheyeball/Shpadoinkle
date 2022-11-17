{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ViewPatterns          #-}
#endif

module Shpadoinkle.JSFFI
  ( JSM
  , MonadJSM
  , JSContextRef
  , liftJSM
  , askJSM
  , runJSM

  , ghcjsOnly

  , Make
  , MakePure
  , make
  , purely

  , type (<:)
  , jsAs
  , jsTo
  , upcast
  , downcast
  , sidecast
  , downcastJSM
  , downcastJSMReticent
  , downcastUnsafe
  , downcastUnsafeReticent

  , (===)

  , JSVal

  , JSString

  , JSObject
  , mkEmptyObject
  , getProp
  , getPropMaybe
  , setProp
  , deleteProp

  , JSKey
  , makeJSKey

  , JSArray

  , JSFunction
  , HSFunction
  , mkFun
  , mkFun'
  , (#)
  , (#-)
  , setTimeout
  , clearTimeout

  , JSArg
  , JSArgs
  , makeJSArgs

  , JSHTMLElement
  , setInnerHTML
  , createElement
  , createTextNode
  , appendChild
  , setId
  , setAttribute
  , getElementById

  , JSBool
  , jsTrue
  , jsFalse

  , JSStorage
  , localStorage
  , sessionStorage
  , getItem
  , setItem

  , jsNull
  , jsUndefined

  , global
  , window
  , document
  , body
  , console

  , toStringLax
  , toTextLax
  , toNumberLax
  , toIntLax
  , toBoolLax

  , eval

  , requestAnimationFrame
  , requestAnimationFrame_
  ) where


import           Control.Category          ((>>>))
import           Control.Monad             (void, (<=<), (>=>))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Functor.Identity     (Identity (..))
import           Data.Maybe                (fromJust)
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable, typeOf)
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
import           GHCJS.Marshal.Pure        (PFromJSVal, pFromJSVal)
import           GHCJS.Nullable            (Nullable (..))
import           GHCJS.Prim                (JSVal)
import qualified GHCJS.Prim                as Ghcjs
import           JavaScript.Array.Internal (SomeJSArray (..), toListIO)
#endif


ghcjsOnly :: a
ghcjsOnly = error "Attempted to perform an operation which is permitted on Ghcjs only. Are you compiling with GHC? The GHC Shpadoinkle API is intended for typechecking only; you must compile to Ghcjs in order to actually run anything that requires Javascript. (This is most things, unless you are using the static backend.)"


--------------------------------------------------------------------------------
-- JSVal and JSM
--------------------------------------------------------------------------------

#ifndef ghcjs_HOST_OS
data JSVal
#endif

type JSM = IO

type MonadJSM = MonadIO

liftJSM :: MonadJSM m => JSM a -> m a
liftJSM = liftIO

-- This all is trivial, a holdover from JSaddle
-- Included only because S11 re-exports it
-- TODO: shoudl they be purged from S11?
type JSContextRef = ()
askJSM :: MonadJSM m => m JSContextRef
askJSM = pure ()
runJSM :: MonadIO m => JSM a -> JSContextRef -> m a
#ifdef ghcjs_HOST_OS
runJSM act _ = liftIO act
#else
runJSM = ghcjsOnly
#endif


--------------------------------------------------------------------------------
-- Refinement hierarchy
--------------------------------------------------------------------------------

-- An instance of @sub <: sup@ is witness to an isomorphism
-- between @sub@ and some subset of @sup@
--
-- Laws:
--
-- prop> (upcast <$> downcast x) `elem` [Nothing, Just x]
-- prop> downcast . upcast = Just
class sub <: sup where
  upcast :: sub -> sup
  downcast :: sup -> Maybe sub

type a ~: b = (a <: b, b <: a)

sidecast :: forall b a. a ~: b => a -> b
sidecast = upcast

-- | Preferred synonym for 'upcast'
jsAs :: forall b a. a <: b => a -> b
jsAs = upcast

-- | Preferred synonym for 'downcastJSM'
jsTo :: forall a b m. (MonadJSM m, Typeable a, Typeable b, a <: b, b <: JSVal) => b -> m a
jsTo = liftJSM . downcastJSM


-- | triple-equals comparison
(===) :: (a <: JSVal, b <: JSVal) => a -> b -> Bool
#ifdef ghcjs_HOST_OS
(===) a b = treq_js (upcast a) (upcast b)

foreign import javascript unsafe
  "$1 === $2"
  treq_js :: JSVal -> JSVal -> Bool
#else
(===) = ghcjsOnly
#endif

-- | reflexivity on @<:@
rxUp :: a -> a
rxUp = id
rxDn :: a -> Maybe a
rxDn = Just

-- | transitivity on @<:@
trUp :: forall b a c. (a <: b, b <: c) => a -> c
trUp = (upcast :: a -> b) >>> (upcast :: b -> c)
trDn :: forall b a c. (a <: b, b <: c) => c -> Maybe a
trDn = (downcast :: c -> Maybe b) >=> (downcast :: b -> Maybe a)

-- This is the reflexive+transitive closure of the (<:) relation.
-- This code is generated by ./generate-closure.js
instance {-gen-} Double <: Double where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSVal <: JSVal where { upcast = rxUp; downcast = rxDn }
instance {-gen-} Int <: Int where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSString <: JSString where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSString <: JSKey where { upcast = trUp @Text; downcast = trDn @Text }
instance {-gen-} Text <: JSVal where { upcast = trUp @JSString; downcast = trDn @JSString }
instance {-gen-} Text <: Text where { upcast = rxUp; downcast = rxDn }
instance {-gen-} Text <: String where { upcast = trUp @JSString; downcast = trDn @JSString }
instance {-gen-} String <: JSVal where { upcast = trUp @JSString; downcast = trDn @JSString }
instance {-gen-} String <: Text where { upcast = trUp @JSString; downcast = trDn @JSString }
instance {-gen-} String <: String where { upcast = rxUp; downcast = rxDn }
instance {-gen-} String <: JSKey where { upcast = trUp @JSString; downcast = trDn @JSString }
instance {-gen-} JSObject <: JSObject where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSKey <: JSKey where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSArray <: JSArray where { upcast = rxUp; downcast = rxDn }
instance {-gen-} [JSVal] <: JSVal where { upcast = trUp @JSArray; downcast = trDn @JSArray }
instance {-gen-} [JSVal] <: [JSVal] where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSFunction <: JSFunction where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSHTMLElement <: JSVal where { upcast = trUp @JSObject; downcast = trDn @JSObject }
instance {-gen-} JSHTMLElement <: JSHTMLElement where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSBool <: JSBool where { upcast = rxUp; downcast = rxDn }
instance {-gen-} Bool <: JSVal where { upcast = trUp @JSBool; downcast = trDn @JSBool }
instance {-gen-} Bool <: Bool where { upcast = rxUp; downcast = rxDn }
instance {-gen-} JSStorage <: JSVal where { upcast = trUp @JSObject; downcast = trDn @JSObject }
instance {-gen-} JSStorage <: JSStorage where { upcast = rxUp; downcast = rxDn }


-- | Perform a downcast. Throws inside JSM if the downcast is invalid.
downcastJSM :: forall sub sup. (Typeable sup, Typeable sub, sub <: sup, sup <: JSVal) => sup -> JSM sub
#ifdef ghcjs_HOST_OS
downcastJSM x =
  case downcast x of
    Just a -> pure a
    Nothing ->
      impossible <$>
        downcastFailure
          (upcast . show $ typeOf (undefined :: sup))
          (upcast . show $ typeOf (undefined :: sub))
          (upcast x)

foreign import javascript unsafe
  "throw 'Failure downcasting from type ' + $1 + ' to type ' + $2 + ' on value: ' + $3"
  downcastFailure :: JSVal -> JSVal -> JSVal -> JSM ()
#else
downcastJSM = ghcjsOnly
#endif


-- | Like 'downcastJSM', but with fewer constraints and worse error reporting
downcastJSMReticent :: sub <: sup => sup -> JSM sub
#ifdef ghcjs_HOST_OS
downcastJSMReticent x =
  case downcast x of
    Just a -> pure a
    Nothing -> impossible <$> downcastFailureReticent

foreign import javascript unsafe
  "throw 'Failure while downcasting (downcastJSMReticent)'"
  downcastFailureReticent :: JSM ()
#else
downcastJSMReticent = ghcjsOnly
#endif


-- | Like 'downcastJSM', but not in 'JSM'
downcastUnsafe :: (Typeable sup, Typeable sub, sub <: sup, sup <: JSVal) => sup -> sub
downcastUnsafe = unsafePerformIO . downcastJSM


-- | Like 'downcastJSMReticent', but not in 'JSM'
downcastUnsafeReticent :: sub <: sup => sup -> sub
downcastUnsafeReticent = fromJust . downcast

-- Used to assert that, although the typesystem thinks we have an 'a',
-- in fact we never will.
impossible :: forall a. a -> (forall b. b)
impossible = unsafeCoerce


-- I don't trust GHCJS' impl
-- Note: this folds errors from 'pFromJSVal' into the result 'Maybe'
#ifdef ghcjs_HOST_OS
fromNullable :: PFromJSVal a => Nullable a -> Maybe a
fromNullable (Nullable jsv) =
  if jsv === jsNull || jsv === jsUndefined
  then Nothing
  else pFromJSVal jsv
#endif


-- | Double is identified with JS numbers
instance Double <: JSVal where
#ifdef ghcjs_HOST_OS
  upcast = Ghcjs.pToJSVal
#else
  upcast = ghcjsOnly
#endif
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fromNullable . downcast_Double_js

foreign import javascript unsafe
  "$r = typeof $1 === 'number' ? $1 : null"
  downcast_Double_js :: JSVal -> Nullable Double
#endif


-- | Int is identified with integral JS numbers
instance Int <: JSVal where
#ifdef ghcjs_HOST_OS
  upcast = Ghcjs.pToJSVal
#else
  upcast = ghcjsOnly
#endif
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fromNullable . downcast_Int_js

foreign import javascript unsafe
  "$r = (Number.isFinite($1) && ($1 | 0) === $1) ? $1 : null"
  downcast_Int_js :: JSVal -> Nullable Int
#endif


--------------------------------------------------------------------------------
-- typeclass Make
--------------------------------------------------------------------------------

-- |
--
-- Class for generic conversion functions which may or may not
-- do actual work.
--
-- This class has no laws.
--
-- This class should be used sparingly.
class Make f b a where
  make :: a -> f b

type MakePure = Make Identity

-- |
--
-- If exists instance To Identity B A, then have both:
-- * to :: A -> Identity B
-- * purely to :: A -> B
purely :: (a -> Identity b) -> (a -> b)
purely = fmap runIdentity



--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------

newtype JSString = JSString { unJSString :: JSVal }
  deriving (Typeable)

instance JSString <: JSVal where
  upcast = unJSString
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fmap JSString . fromNullable . downcast_JSString_js

foreign import javascript unsafe
  "$r = typeof $1 === 'string' ? $1 : null"
  downcast_JSString_js :: JSVal -> Nullable JSVal
#endif


-- | Text is identified with JS strings
instance Text <: JSString where
#ifdef ghcjs_HOST_OS
  upcast = JSString . Ghcjs.toJSString . T.unpack
#else
  upcast = ghcjsOnly
#endif
  downcast = Just . sidecast

instance JSString <: Text where
#ifdef ghcjs_HOST_OS
  upcast = Ghcjs.pFromJSVal . unJSString
#else
  upcast = ghcjsOnly
#endif
  downcast = Just . sidecast


-- | String is identified with JS strings
instance String <: JSString where
#ifdef ghcjs_HOST_OS
  upcast = JSString . Ghcjs.toJSString
#else
  upcast = ghcjsOnly
#endif
  downcast = fmap T.unpack . downcast

instance JSString <: String where
#ifdef ghcjs_HOST_OS
  upcast = Ghcjs.pFromJSVal . unJSString
#else
  upcast = ghcjsOnly
#endif
  downcast = Just . sidecast



instance IsString JSString where
  fromString = sidecast

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

-- |
--
-- Refines 'JSVal' to only objects
--
-- A value 'x' is considered to be an object if the following condition holds:
-- prop> (typeof x === 'object' && x !== null) || (typeof x === 'function')
newtype JSObject = JSObject { unJSObject :: JSVal }
  deriving (Typeable)


instance JSObject <: JSVal where
  upcast = unJSObject
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fmap JSObject . fromNullable . downcast_JSObject_js

foreign import javascript unsafe
  "$r = (typeof $1 === 'object' && $1 !== null) || typeof $1 === 'function' ? $1 : null"
  downcast_JSObject_js :: JSVal -> Nullable JSVal
#endif


mkEmptyObject :: MonadJSM m => m JSObject
#ifdef ghcjs_HOST_OS
mkEmptyObject = JSObject <$> liftJSM mkEmptyObject_js
foreign import javascript unsafe
  "$r = ({})"
  mkEmptyObject_js :: JSM JSVal
#else
mkEmptyObject = ghcjsOnly
#endif


-- Get a property off of a 'JSObject' and then 'downcast' it.
--
-- The choice to use '<:' here instead of a more generic class, like for
-- instance a generic 'FromJSVal' class, means that the downcast has limited
-- power. The downcast itself must have no effects and must be injective (unless
-- it fails); in other words, it "can't actually do any work".
getProp ::
  forall res m obj key.
  ( MonadJSM m
  , obj <: JSObject
  , Make m JSKey key
  , res <: JSVal
  , Typeable res
  ) => key -> obj -> m res
#ifdef ghcjs_HOST_OS
getProp k o = do
  k' :: JSVal <- upcast <$> makeJSKey k
  let o' = jsAs @JSVal . jsAs @JSObject $ o
  liftJSM $ downcastJSM =<< getProp_js k' o'

foreign import javascript unsafe
  "$r = $2[$1]"
  getProp_js :: JSVal -> JSVal -> JSM JSVal
#else
getProp = ghcjsOnly
#endif


getPropMaybe ::
  forall res m obj key.
  ( MonadJSM m
  , obj <: JSObject
  , Make m JSKey key
  , res <: JSVal
  , Typeable res
  ) => key -> obj -> m (Maybe res)
getPropMaybe k v = downcast <$> getProp @JSVal k v


setProp :: (MonadJSM m, Make m JSKey key, val <: JSVal, obj <: JSObject) => key -> val -> obj -> m ()
#ifdef ghcjs_HOST_OS
setProp k v o = do
  k' <- pure . upcast . unJSKey =<< makeJSKey k
  let v' = upcast $ v
  let o' = unJSObject $ upcast o
  liftJSM $ setProp_js k' v' o'

foreign import javascript unsafe
  "$3[$1] = $2"
  setProp_js :: JSVal -> JSVal -> JSVal -> JSM ()
#else
setProp = ghcjsOnly
#endif


deleteProp :: (MonadJSM m, Make m JSKey key, obj <: JSObject) => key -> obj -> m ()
#ifdef ghcjs_HOST_OS
deleteProp k o = do
  k' <- pure . upcast . unJSKey =<< makeJSKey k
  let o' = unJSObject $ upcast o
  liftJSM $ deleteProp_js o' k'

foreign import javascript unsafe
  "delete $1[$2]"
  deleteProp_js :: JSVal -> JSVal -> JSM ()
#else
deleteProp = ghcjsOnly
#endif


--------------------------------------------------------------------------------
-- Object keys
--------------------------------------------------------------------------------

-- | Represents a string or symbol
newtype JSKey = JSKey { unJSKey :: JSVal }
  deriving (Typeable)

instance JSKey <: JSVal where
  upcast = unJSKey
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fmap JSKey . fromNullable . downcast_JSKey_js

foreign import javascript unsafe
  "$r = typeof $1 === 'string' || typeof $1 === 'symbol' ? $1 : null"
  downcast_JSKey_js :: JSVal -> Nullable JSVal
#endif

instance Text <: JSKey where
  upcast = JSKey . upcast
  downcast = downcast . id @JSVal . upcast


makeJSKey :: Make m JSKey a => a -> m JSKey
makeJSKey = make

instance Applicative m => Make m JSKey JSKey where
  make = pure

instance Applicative m => Make m JSKey JSString where
  make = pure . JSKey . id @JSVal . upcast

instance Applicative m => Make m JSKey String where
  make = pure . JSKey . id @JSVal . upcast

instance Applicative m => Make m JSKey Text where
  make = pure . JSKey . id @JSVal . upcast

instance Applicative m => Make m JSKey Int where
  make = make . show


--------------------------------------------------------------------------------
-- Arrays
--------------------------------------------------------------------------------

newtype JSArray = JSArray { unJSArray :: JSObject }
  deriving (Typeable)


instance JSArray <: JSVal where
  upcast = upcast . unJSArray
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fmap (JSArray . JSObject) . fromNullable . downcast_JSArray_js . upcast

foreign import javascript unsafe
  "$r = Array.isArray($1) ? $1 : null"
  downcast_JSArray_js :: JSVal -> Nullable JSVal
#endif


instance JSArray <: [JSVal] where
  downcast = Just . sidecast
#ifndef ghcjs_HOST_OS
  upcast = ghcjsOnly
#else
  upcast (JSArray ar) = unsafePerformIO $ do
    len <- toIntLax =<< getProp "length" ar
    for [0 :: Int .. len - 1] $ \idx -> getProp idx ar
#endif


instance [JSVal] <: JSArray where
  downcast = Just . sidecast
#ifndef ghcjs_HOST_OS
  upcast = ghcjsOnly
#else
  upcast xs = unsafePerformIO $ do
    ar <- pure . JSObject =<< newArray_js =<< (pure . upcast . length) xs
    for (zip [0..] xs) $ \(idx, val) -> setProp (idx :: Int) val ar
    pure $ JSArray ar

foreign import javascript unsafe
  "$r = Array.from(Array($1))"
      -- nb. "pre-allocate" for possible perf boost. unbenchmarked.
  newArray_js :: JSVal -> JSM JSVal
#endif


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

newtype JSFunction = JSFunction { unJSFunction :: JSVal }
  deriving (Typeable)


instance JSFunction <: JSVal where
  upcast = unJSFunction
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fmap JSFunction . fromNullable . downcast_JSFunction_js

foreign import javascript unsafe
  "$r = $1 instanceof Function ? $1 : null"
  downcast_JSFunction_js :: JSVal -> Nullable JSVal
#endif


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


-- | Like @mkFun@ but for functions which don't use the self-reference
mkFun' :: MonadJSM m => ([JSVal] -> JSM ()) -> m JSFunction
mkFun' fun = mkFun (\_fun _this args -> fun args)


setTimeout :: MonadJSM m => Int -> JSFunction -> m Int
#ifdef ghcjs_HOST_OS
setTimeout delay (JSFunction fun) =
    liftJSM $ toIntLax =<< setTimeout_js fun (upcast delay)

foreign import javascript unsafe
  "$r = globalThis.setTimeout($1, $2)"
  setTimeout_js :: JSVal -> JSVal -> JSM JSVal
#else
setTimeout = ghcjsOnly
#endif


clearTimeout :: MonadJSM m => Int -> m ()
#ifdef ghcjs_HOST_OS
clearTimeout tid =
    liftJSM $ clearTimeout_js (upcast tid)

foreign import javascript unsafe
  "globalThis.clearTimeout($1)"
  clearTimeout_js :: JSVal -> JSM ()
#else
clearTimeout = ghcjsOnly
#endif


--------------------------------------------------------------------------------
-- Function arguments
--------------------------------------------------------------------------------

newtype JSArg = JSArg { unJSArg :: JSVal }

mkArgJSVal :: forall a m. (Functor m, Make m JSArg a) => a -> m JSVal
mkArgJSVal = fmap unJSArg . make

-- nb
-- These incoherent instances are non-problematic because there is no
-- instance 'Maybe X <: JSVal' for any X. This means that these instances
-- will never actually overlap (but GHC doesn't know that)

instance {-# INCOHERENT #-} (Applicative m, a <: JSVal) => Make m JSArg a where
  make = pure . JSArg . upcast

instance {-# INCOHERENT #-} (Applicative m, Make m JSArg a) => Make m JSArg (Maybe a) where
  make = \case
    Nothing -> make jsUndefined
    Just a -> make a


-- | Isomorphic with 'JSArray', but has different instances
newtype JSArgs = JSArgs { unJSArgs :: JSArray }

-- nb
-- There is no 'JSArgs <: JSVal' instance.
-- This is intentional -- although such an instance *could* exist,
-- the intent of JSArgs is as a newtype for conveniently creating
-- arguments, not as a subtype of JSVal.

makeJSArgs :: forall a m. Make m JSArgs a => a -> m JSArgs
makeJSArgs = make

instance Applicative m => Make m JSArgs JSArgs where
  make = pure

instance {-# OVERLAPPABLE #-} (Applicative m, Make m JSArg a) => Make m JSArgs a where
  make = mkArgJSVal >>> fmap ((:[]) >>> upcast >>> JSArgs)

instance (Applicative m, Make m JSArg a) => Make m JSArgs [a] where
  make = traverse mkArgJSVal >>> fmap (upcast >>> JSArgs)

instance (Monad m, Make m JSArg a1, Make m JSArg a2) => Make m JSArgs (a1, a2) where
  make (a1, a2) =
    (makeJSArgs =<<) $
      (\b1 b2 -> [b1, b2]) <$> mkArgJSVal a1 <*> mkArgJSVal a2

instance (Monad m, Make m JSArg a1, Make m JSArg a2, Make m JSArg a3) => Make m JSArgs (a1, a2, a3) where
  make (a1, a2, a3) =
    (makeJSArgs =<<) $
      (\b1 b2 b3 -> [b1, b2, b3]) <$> mkArgJSVal a1 <*> mkArgJSVal a2 <*> mkArgJSVal a3


-- | Call a JS method
(#) ::
  ( MonadJSM m
  , this <: JSObject
  , Make m JSKey prop
  , Make m JSArgs args
  , res <: JSVal
  , Typeable res
  ) => this -> prop -> args -> m res
#ifdef ghcjs_HOST_OS
(#) this prop args = do
  this' <- (upcast :: JSObject -> JSVal) . upcast <$> pure this
  prop' <- unJSKey <$> makeJSKey prop
  args' <- upcast . unJSArgs <$> makeJSArgs args
  liftJSM $ downcastJSM =<< unsafeCall_js this' prop' args'

foreign import javascript unsafe
  "$r = (function (it) { return it[$2].apply(it, $3); })($1)"
  unsafeCall_js :: JSVal -> JSVal -> JSVal -> JSM JSVal
#else
(#) = ghcjsOnly
#endif
infixr 2 #


-- |
--
-- Like '#' but returns '()'
--
-- Using '#' and 'void' won't work well due to '#' being parametric over its return type
(#-) ::
  ( MonadJSM m
  , this <: JSObject
  , Make m JSKey prop
  , Make m JSArgs args
  ) => this -> prop -> args -> m ()
(#-) this prop args = do
  r :: JSVal <- this # prop $ args
  pure ()
infixr 2 #-


--------------------------------------------------------------------------------
-- HTMLElement
--------------------------------------------------------------------------------

newtype JSHTMLElement = JSHTMLElement { unJSHTMLElement :: JSObject }
  deriving (Typeable)

instance JSHTMLElement <: JSObject where
  upcast = unJSHTMLElement
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fmap (JSHTMLElement . JSObject) . fromNullable . downcast_JSHTMLElement_js . unJSObject

foreign import javascript unsafe
  "$r = $1 instanceof HTMLElement ? $1 : null"
  downcast_JSHTMLElement_js :: JSVal -> Nullable JSVal
#endif

setInnerHTML :: (MonadJSM m, s <: JSString) => s -> JSHTMLElement -> m ()
#ifdef ghcjs_HOST_OS
setInnerHTML str el = liftJSM $ setInnerHTML_js (upcast el) (upcast . id @JSString . upcast $ str)
foreign import javascript unsafe
  "$1.innerHTML = $2"
  setInnerHTML_js :: JSVal -> JSVal -> JSM ()
#else
setInnerHTML = ghcjsOnly
#endif

createElement :: MonadJSM m => Text -> m JSHTMLElement
#ifdef ghcjs_HOST_OS
createElement name = liftJSM $ (JSHTMLElement . JSObject) <$> createElement_js (upcast name)
foreign import javascript unsafe
  "$r = document.createElement($1)"
  createElement_js :: JSVal -> JSM JSVal
#else
createElement = ghcjsOnly
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

-- |
--
-- nb. @ch <: JSVal@ is not as strong as the WebAPI requires; be careful!
appendChild :: (MonadJSM m, ch <: JSVal) => ch -> JSHTMLElement -> m ()
#ifdef ghcjs_HOST_OS
appendChild child parent = liftJSM $ appendChild_js (upcast parent) (upcast child)
foreign import javascript unsafe
  "$1.appendChild($2)"
  appendChild_js :: JSVal -> JSVal -> JSM ()
#else
appendChild = ghcjsOnly
#endif

setId :: MonadJSM m => Text -> JSHTMLElement -> m ()
#ifdef ghcjs_HOST_OS
setId newId el = liftJSM $ setId_js (upcast el) (upcast newId)
foreign import javascript unsafe
  "$1.id = $2"
  setId_js :: JSVal -> JSVal -> JSM ()
#else
setId = ghcjsOnly
#endif

setAttribute :: MonadJSM m => Text -> Text -> JSHTMLElement -> m ()
#ifdef ghcjs_HOST_OS
setAttribute attr val el = liftJSM $ setAttribute_js (upcast el) (upcast attr) (upcast val)
foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  setAttribute_js :: JSVal -> JSVal -> JSVal -> JSM ()
#else
setAttribute = ghcjsOnly
#endif

getElementById :: MonadJSM m => Text -> m (Maybe JSHTMLElement)
#ifdef ghcjs_HOST_OS
getElementById eid = do
  el <- liftJSM $ getElementById_js (upcast eid)
  pure $ JSHTMLElement . JSObject <$> fromNullable el

foreign import javascript unsafe
  "$r = document.getElementById($1)"
  getElementById_js :: JSVal -> JSM (Nullable JSVal)
#else
getElementById = ghcjsOnly
#endif


--------------------------------------------------------------------------------
-- Booleans
--------------------------------------------------------------------------------

newtype JSBool = JSBool { unJSBool :: JSVal }
  deriving (Typeable)

instance JSBool <: JSVal where
  upcast = unJSBool
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fmap JSBool . fromNullable . downcast_JSBool_js

foreign import javascript unsafe
  "$r = typeof $1 === 'boolean' ? $1 : null"
  downcast_JSBool_js :: JSVal -> Nullable JSVal
#endif

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


instance Bool <: JSBool where
#ifdef ghcjs_HOST_OS
  upcast = JSBool . Ghcjs.pToJSVal
#else
  upcast = ghcjsOnly
#endif
  downcast = Just . sidecast

instance JSBool <: Bool where
#ifdef ghcjs_HOST_OS
  upcast = Ghcjs.pFromJSVal . unJSBool
#else
  upcast = ghcjsOnly
#endif
  downcast = Just . sidecast


--------------------------------------------------------------------------------
-- Units
--------------------------------------------------------------------------------

-- | Haskell Unit is identified with JS undefined
instance () <: JSVal where
  upcast = pure jsUndefined
  downcast x = if x === jsUndefined then Just () else Nothing

jsUndefined :: JSVal
#ifdef ghcjs_HOST_OS
jsUndefined = unsafePerformIO jsUndefined_js
foreign import javascript unsafe
  "$r = undefined"
  jsUndefined_js :: JSM JSVal
#else
jsUndefined = ghcjsOnly
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


--------------------------------------------------------------------------------
-- Storage API
--------------------------------------------------------------------------------

newtype JSStorage = JSStorage { unJSStorage :: JSObject }
  deriving (Typeable)

instance JSStorage <: JSObject where
  upcast = unJSStorage
#ifndef ghcjs_HOST_OS
  downcast = ghcjsOnly
#else
  downcast = fmap (JSStorage . JSObject) . fromNullable . downcast_JSStorage_js . upcast

foreign import javascript unsafe
  "$r = $1 instanceof Storage ? $1 : null"
  downcast_JSStorage_js :: JSVal -> Nullable JSVal
#endif

localStorage, sessionStorage :: JSStorage
#ifdef ghcjs_HOST_OS
localStorage = unsafePerformIO $ getProp "localStorage" window
sessionStorage = unsafePerformIO $ getProp "sessionStorage" window
#else
localStorage = ghcjsOnly
sessionStorage = ghcjsOnly
#endif

setItem :: (MonadJSM m, key <: JSString, val <: JSString) => key -> val -> JSStorage -> m ()
#ifdef ghcjs_HOST_OS
setItem key val store = do
  let key' = (upcast :: JSString -> JSVal) . upcast $ key
  let val' = (upcast :: JSString -> JSVal) . upcast $ val
  liftJSM $ setItem_js (upcast store) key' val'

foreign import javascript unsafe
  "$1.setItem($2, $3)"
  setItem_js :: JSVal -> JSVal -> JSVal -> JSM ()
#else
setItem = ghcjsOnly
#endif

getItem :: (MonadJSM m, key <: JSString) => key -> JSStorage -> m (Maybe JSString)
#ifdef ghcjs_HOST_OS
getItem key store = do
  let key' = (upcast :: JSString -> JSVal) . upcast $ key
  mStr <- liftJSM $ fromNullable <$> getItem_js (upcast store) key'
  pure $ JSString <$> mStr

foreign import javascript unsafe
  "$r = $1.getItem($2)"
  getItem_js :: JSVal -> JSVal -> JSM (Nullable JSVal)
#else
getItem = ghcjsOnly
#endif


--------------------------------------------------------------------------------
-- Lax conversions
--------------------------------------------------------------------------------

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

toStringLax :: MonadJSM m => JSVal -> m String
toStringLax = fmap T.unpack . toTextLax

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


--------------------------------------------------------------------------------
-- Global references
--------------------------------------------------------------------------------

-- | Reference to 'globalThis'
global :: JSObject
#ifdef ghcjs_HOST_OS
global = unsafePerformIO $ downcastJSM =<< global_js
foreign import javascript unsafe
  "$r = globalThis"
  global_js :: JSM JSVal
#else
global = ghcjsOnly
#endif

-- | Global reference to 'window'
window :: JSObject
window = unsafePerformIO $ getProp "window" global

-- | Global reference to 'document'
document :: JSObject
document = unsafePerformIO $ getProp "document" global

-- |
--
-- Global reference to 'body'
--
-- **Warning**: if this is evaluated before 'globalThis.body' exists,
-- it will not return the correct value. You can use '<script defer' to
-- ensure this doesn't happen.
body :: JSHTMLElement
body = unsafePerformIO $ getProp "body" document

-- | Global reference to 'console'
console :: JSObject
console = unsafePerformIO $ getProp "console" global


--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

eval :: forall s m. (MonadJSM m, s <: JSString) => s -> m JSVal
#ifdef ghcjs_HOST_OS
eval (upcast -> s) = liftJSM (eval_js s)
foreign import javascript unsafe
  "$r = eval($1)"
  eval_js :: JSString -> IO JSVal
#else
eval = ghcjsOnly
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

