{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


{-|
   Shpadoinkle is an abstract frontend programming model, with one-way data flow, and a single source of truth.
   This module provides a parsimonious implementation of Shpadoinkle with few implementation details.
-}


module Shpadoinkle.Core (
  -- * Base Types
  Html(..), Prop(..), Props(..), fromProps, toProps
  -- ** Prop Constructors
  , dataProp, flagProp, textProp, listenerProp, bakedProp
  -- *** Listeners
  , listenRaw, listen, listenM, listenM_, listenC, listener
  -- ** Html Constructors
  , h, baked, text
  -- ** Hoists
  , hoistHtml, hoistProp
  -- ** Catamorphisms
  , cataH, cataProp
  -- ** Utilities
  , mapProps, injectProps, eitherH
  -- * JSVal Wrappers
  , RawNode(..), RawEvent(..)
  -- * Backend Interface
  , Backend (..)
  , type (~>)
  -- * The Shpadoinkle Primitive
  , shpadoinkle
  -- * Re-Exports
  , JSM, MonadJSM, askJSM, runJSM, MonadUnliftIO(..), UnliftIO(..), liftJSM
  , module UnliftIO.STM
  ) where


import qualified Control.Categorical.Functor   as F
import           Control.Category              ((.))
import           Control.PseudoInverseCategory (EndoIso (..),
                                                HasHaskFunctors (fmapA),
                                                PIArrow (piendo, piiso),
                                                PseudoInverseCategory (piinverse),
                                                ToHask (piapply))
import           Data.Kind                     (Type)
import           Data.Map                      as M (Map, singleton, toList,
                                                     unionWithKey)
import           Data.String                   (IsString (..))
import           Data.Text                     (Text, pack)
import           GHCJS.DOM.Types               (JSM, MonadJSM, liftJSM)
import           Language.Javascript.JSaddle   (FromJSVal (..), JSVal,
                                                ToJSVal (..), askJSM, runJSM)
import           Prelude                       hiding ((.))
import           UnliftIO                      (MonadUnliftIO (..),
                                                UnliftIO (..))
import           UnliftIO.STM                  (STM, TVar, atomically,
                                                modifyTVar, newTVarIO, readTVar,
                                                readTVarIO, retrySTM, writeTVar)


import           Shpadoinkle.Continuation      (Continuation, Continuous (..),
                                                causes, eitherC, hoist, impur,
                                                pur, shouldUpdate)


-- | This is the core type in Backend.
-- Please note, this is NOT the Virtual DOM used by Backend.
-- This type backs a DSL that is then /interpreted/ into Virtual DOM
-- by the Backend of your choosing. HTML comments are not supported.
-- This is Church encoded for performance reasons.
newtype Html m a = Html
  { unHtml
      :: forall r. (Text -> [(Text, Prop m a)] -> [r] -> r)
      -> (JSM (RawNode, STM (Continuation m a)) -> r)
      -> (Text -> r)
      -> r
  }


-- | Properties of a DOM node. Backend does not use attributes directly,
-- but rather is focused on the more capable properties that may be set on a DOM
-- node in JavaScript. If you wish to add attributes, you may do so
-- by setting its corresponding property.
data Prop :: (Type -> Type) -> Type -> Type where
  -- | A data property, these do NOT appear in static rendering
  PData :: JSVal -> Prop m a
  -- | A text property
  PText :: Text -> Prop m a
  -- | A boolean property
  PFlag :: Bool -> Prop m a
  -- | Bake a custom property
  -- The STM Monad will be called recursively.
  -- The semantics here is roughly an event stream of continuations.
  PPotato :: (RawNode -> JSM (STM (Continuation m a))) -> Prop m a
  -- | Event listeners are provided with the 'RawNode' target, and the 'RawEvent', and may perform
  -- a monadic action such as a side effect. This is the one and only place where you may
  -- introduce a custom monadic action. The JSM to compute the Continuation must be
  -- synchronous and non-blocking; otherwise race conditions may result from a Pure
  -- Continuation which sets the state based on a previous state captured by the closure.
  -- Such Continuations must be executed synchronously during event propagation,
  -- and that may not be the case if the code to compute the Continuation of some
  -- listener is blocking.
  PListener :: (RawNode -> RawEvent -> JSM (Continuation m a)) -> Prop m a


instance Eq (Prop m a) where
  x == y = case (x,y) of
    (PText x', PText y') -> x' == y'
    (PFlag x', PFlag y') -> x' == y'
    _                    -> False


-- | Construct a listener from its name and a simple monadic event handler.
listenM :: Applicative m => Text -> m (a -> a) -> (Text, Prop m a)
listenM k = listenC k . impur


-- | Construct a listener from its name and a simple stateless monadic event handler.
listenM_ :: Applicative m => Text -> m () -> (Text, Prop m a)
listenM_ k = listenC k . causes


newtype Props m a = Props { getProps :: Map Text (Prop m a) }


toProps :: Applicative m => [(Text, Prop m a)] -> Props m a
toProps = foldMap $ Props . uncurry singleton


fromProps :: Props m a -> [(Text, Prop m a)]
fromProps = M.toList . getProps


instance Applicative m => Semigroup (Props m a) where
  Props xs <> Props ys = Props (unionWithKey go xs ys)
    where
      go k old new = case (old, new) of
        (PText t, PText t') | k == "className" -> PText (t <> " " <> t')
        (PText t, PText t') | k == "style"     -> PText (t <> "; " <> t')
        (PListener l, PListener l')            -> PListener $
           \raw evt -> mappend <$> l raw evt <*> l' raw evt
        _                                      -> new


instance Applicative m => Monoid (Props m a) where
  mempty = Props mempty


-- | If you can provide a Natural Transformation from one Functor to another
-- then you may change the action of 'Html'.
hoistHtml :: Functor m => (m ~> n) -> Html m a -> Html n a
hoistHtml f (Html h') = Html $ \n p t -> h'
  (\t' ps cs -> n t' (fmap (hoistProp f) <$> ps) cs) (p . fmap (fmap (fmap (hoist f)))) t
{-# INLINE hoistHtml #-}


-- | If you can provide a Natural Transformation from one Functor to another
-- then you may change the action of 'Prop'.
hoistProp :: Functor m => (m ~> n) -> Prop m a -> Prop n a
hoistProp f = \case
  PListener g -> PListener $ \x -> fmap (hoist f) . g x
  PData t     -> PData t
  PText t     -> PText t
  PFlag t     -> PFlag t
  PPotato p   -> PPotato $ fmap (fmap (hoist f)) . p
{-# INLINE hoistProp #-}


-- | Strings are overloaded as HTML text nodes:
-- @
--   "hiya" = TextNode "hiya"
-- @
instance IsString (Html m a) where
  fromString = text . pack
  {-# INLINE fromString #-}


-- | Strings are overloaded as text props:
-- @
--   ("id", "foo") = ("id", PText "foo")
-- @
instance IsString (Prop m a) where
  fromString = PText . pack
  {-# INLINE fromString #-}


-- | @Html m@ is a functor in the EndoIso category, where the objects are
--   types and the morphisms are EndoIsos.
instance Applicative m => F.Functor EndoIso EndoIso (Html m) where
  map (EndoIso f g i) = EndoIso (mapC . piapply $ map' (piendo f))
                                (mapC . piapply $ map' (piiso g i))
                                (mapC . piapply $ map' (piiso i g))
    where map' :: EndoIso a b -> EndoIso (Continuation m a) (Continuation m b)
          map' = F.map
  {-# INLINE map #-}


-- | Prop is a functor in the EndoIso category, where the objects are types
--  and the morphisms are EndoIsos.
instance Applicative m => F.Functor EndoIso EndoIso (Prop m) where
  map :: forall a b. EndoIso a b -> EndoIso (Prop m a) (Prop m b)
  map f = EndoIso id mapFwd mapBack
    where f' :: EndoIso (Continuation m a) (Continuation m b)
          f' = F.map f

          mapFwd :: Prop m a -> Prop m b
          mapFwd (PData t)     = PData t
          mapFwd (PText t)     = PText t
          mapFwd (PFlag t)     = PFlag t
          mapFwd (PListener g) = PListener $ \r e -> piapply f' <$> g r e
          mapFwd (PPotato p)   = PPotato $ fmap (fmap (piapply f')) . p


          mapBack :: Prop m b -> Prop m a
          mapBack (PData t)     = PData t
          mapBack (PText t)     = PText t
          mapBack (PFlag t)     = PFlag t
          mapBack (PListener g) = PListener $ \r e -> piapply (piinverse f') <$> g r e
          mapBack (PPotato b)   = PPotato $ fmap (fmap (piapply (piinverse f'))) . b
  {-# INLINE map #-}


-- | Given a lens, you can change the type of an Html by using the lens
--   to convert the types of the Continuations inside it.
instance Continuous Html where
  mapC f (Html h') = Html $ \n p t -> h' (\t' ps cs -> n t' (fmap (mapC f) <$> ps) cs)
         (p . fmap (fmap (fmap (mapC f)))) t
  {-# INLINE mapC #-}


-- | Props is a functor in the EndoIso category, where the objects are
--  types and the morphisms are EndoIsos.
instance Applicative m => F.Functor EndoIso EndoIso (Props m) where
  map f = piiso Props getProps . fmapA (F.map f) . piiso getProps Props
  {-# INLINE map #-}


-- | Given a lens, you can change the type of a Props by using the lens
--   to convert the types of the Continuations inside.
instance Continuous Props where
  mapC f = Props . fmap (mapC f) . getProps
  {-# INLINE mapC #-}


-- | Given a lens, you can change the type of a Prop by using the
--   lens to convert the types of the Continuations which it contains
--   if it is a listener.
instance Continuous Prop where
  mapC _ (PData t)     = PData t
  mapC _ (PText t)     = PText t
  mapC _ (PFlag b)     = PFlag b
  mapC f (PListener g) = PListener $ \r -> fmap f . g r
  mapC f (PPotato b)   = PPotato $ fmap (fmap f) . b
  {-# INLINE mapC #-}


-- | Create a data property.
dataProp :: JSVal -> Prop m a
dataProp = PData
{-# INLINE dataProp #-}


-- | Create a text property.
textProp :: Text -> Prop m a
textProp = PText
{-# INLINE textProp #-}


flagProp :: Bool -> Prop m a
flagProp = PFlag
{-# INLINE flagProp #-}


-- | Create an event listener property.
listenerProp :: (RawNode -> RawEvent -> JSM (Continuation m a)) -> Prop m a
listenerProp = PListener
{-# INLINE listenerProp #-}


-- | Create a delicious proptato.
bakedProp :: (RawNode -> JSM (STM (Continuation m a))) -> Prop m a
bakedProp = PPotato
{-# INLINE bakedProp #-}


-- | Transform a p-algebra into a p-catamorphism. This is like polymorphic pattern matching.
cataProp
  :: (JSVal -> b)
  -> (Text -> b)
  -> (Bool -> b)
  -> ((RawNode -> RawEvent -> JSM (Continuation m a)) -> b)
  -> ((RawNode -> JSM (STM (Continuation m a))) -> b)
  -> Prop m a
  -> b
cataProp d t f l p = \case
  PData     x -> d x
  PText     x -> t x
  PFlag     x -> f x
  PListener x -> l x
  PPotato   x -> p x


-- | Construct an HTML element JSX-style.
h :: Text -> [(Text, Prop m a)] -> [Html m a] -> Html m a
h t ps cs = Html $ \a b c -> a t ps ((\(Html h') -> h' a b c) <$> cs)
{-# INLINE h #-}


-- | Construct a 'Potato' from a 'JSM' action producing a 'RawNode'.
baked :: JSM (RawNode, STM (Continuation m a)) -> Html m a
baked jr = Html $ \_ p _ -> p jr
{-# INLINE baked #-}


-- | Construct a text node.
text :: Text -> Html m a
text t = Html $ \_ _ f -> f t
{-# INLINE text #-}


-- | Construct an HTML element out of heterogeneous alternatives.
eitherH :: Applicative m => (a -> Html m a) -> (b -> Html m b) -> Either a b -> Html m (Either a b)
eitherH = eitherC
{-# INLINE eitherH #-}


-- | Fold an HTML element, i.e. transform an h-algebra into an h-catamorphism.
cataH :: (Text -> [(Text, Prop m a)] -> [b] -> b)
      -> (JSM (RawNode, STM (Continuation m a)) -> b)
      -> (Text -> b)
      -> Html m a -> b
cataH f g h' (Html h'') = h'' f g h'


-- | Natural Transformation
type m ~> n = forall a. m a -> n a


-- | A DOM node reference.
-- Useful for building baked potatoes and binding a Backend view to the page
newtype RawNode  = RawNode  { unRawNode  :: JSVal }
instance ToJSVal   RawNode where toJSVal   = return . unRawNode
instance FromJSVal RawNode where fromJSVal = return . Just . RawNode


-- | A raw event object reference
newtype RawEvent = RawEvent { unRawEvent :: JSVal }
instance ToJSVal   RawEvent where toJSVal   = return . unRawEvent
instance FromJSVal RawEvent where fromJSVal = return . Just . RawEvent


-- | Strings are overloaded as the class property:
-- @
--   "active" = ("className", PText "active")
-- @
instance {-# OVERLAPPING #-} IsString [(Text, Prop m a)] where
  fromString = pure . ("className", ) . textProp . pack
  {-# INLINE fromString #-}


-- | Construct a simple listener property that will perform an action.
listener :: Continuation m a -> Prop m a
listener = listenerProp . const . const . return
{-# INLINE listener #-}


-- | Construct a listener from its name and an event handler.
listenRaw :: Text -> (RawNode -> RawEvent -> JSM (Continuation m a)) -> (Text, Prop m a)
listenRaw k = (,) k . listenerProp
{-# INLINE listenRaw #-}


-- | Construct a listener from its name and an event handler.
listenC :: Text -> Continuation m a -> (Text, Prop m a)
listenC k = listenRaw k . const . const . return
{-# INLINE listenC #-}


-- | Construct a listener from its 'Text' name and an output value.
listen :: Text -> (a -> a) -> (Text, Prop m a)
listen k = listenC k . pur
{-# INLINE listen #-}


-- | Transform the properties of some Node. This has no effect on 'TextNode's or 'Potato'es.
mapProps :: ([(Text, Prop m a)] -> [(Text, Prop m a)]) -> Html m a -> Html m a
mapProps f (Html h') = Html $ \n p t -> h' (\t' ps cs -> n t' (f ps) cs) p t
{-# INLINE mapProps #-}


-- | Inject props into an existing 'Node'.
injectProps :: [(Text, Prop m a)] -> Html m a -> Html m a
injectProps ps = mapProps (<> ps)
{-# INLINE injectProps #-}


-- | The Backend class describes a backend that can render 'Html'.
-- Backends are generally Monad Transformers @b@ over some Monad @m@.
--
-- prop> patch raw Nothing >=> patch raw Nothing = patch raw Nothing
class Backend b m a | b m -> a where
  -- | VNode type family allows backends to have their own Virtual DOM.
  -- As such we can change out the rendering of our Backend view
  -- with new backends without updating our view logic.
  type VNode b m
  -- | A backend must be able to interpret 'Html' into its own internal Virtual DOM.
  interpret
    :: (m ~> JSM)
    -- ^ Natural transformation for some @m@ to 'JSM'
    -- (this is how a Backend gets access to 'JSM' to perform the rendering side effects)
    -> Html (b m) a
    -- ^ 'Html' to interpret
    -> b m (VNode b m)
    -- ^ Effect producing the Virtual DOM representation

  -- | A Backend must be able to patch the 'RawNode' containing the view, with a
  -- new view if the Virtual DOM changed.
  patch
    :: RawNode
    -- ^ The container for rendering the Backend view
    -> Maybe (VNode b m)
    -- ^ Perhaps there is a previous Virtual DOM to diff against. The value will be 'Nothing' on the first run.
    -> VNode b m
    -- ^ New Virtual DOM to render
    -> b m (VNode b m)
    -- ^ Effect producing an updated Virtual DOM. This is not needed by all backends.
    -- Some JavaScript-based backends need to do this for the next tick. Regardless, whatever
    -- 'VNode' the effect produces will be passed as the previous Virtual DOM on the next render.

  -- | A Backend may perform some imperative setup steps.
  setup :: JSM () -> JSM ()


-- | The core view instantiation function
-- combines a backend, a territory, and a model
-- and renders the Backend view to the page.
shpadoinkle
  :: forall b m a
   . Backend b m a => Monad (b m) => Eq a
  => (m ~> JSM)
  -- ^ How to get to JSM?
  -> (TVar a -> b m ~> m)
  -- ^ What backend are we running?
  -> TVar a
  -- ^ How can we know when to update?
  -> (a -> Html (b m) a)
  -- ^ How should the HTML look?
  -> b m RawNode
  -- ^ Where do we render?
  -> JSM ()
shpadoinkle toJSM toM model view stage = setup @b @m @a $ do

  c <- j stage
  initial <- readTVarIO model
  n <- go c Nothing initial
  () <$ shouldUpdate (go c . Just) n model

  where

  j :: b m ~> JSM
  j = toJSM . toM model

  go :: RawNode -> Maybe (VNode b m) -> a -> JSM (VNode b m)
  go c n a = j $ patch c n =<< interpret toJSM (view a)

