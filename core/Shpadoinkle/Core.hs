{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BangPatterns           #-}
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
  Html(..), Prop(..)
  -- ** Prop Constructors
  , dataProp, flagProp, textProp, listenerProp, bakedProp
  -- *** Listeners
  , listenRaw, listen, listenM, listenM_, listenC, listener
  -- ** Html Constructors
  , h, baked, text
  -- ** Html Lenses
  , props, children, name, textContent
  -- ** Hoists
  , hoistHtml, hoistProp
  -- ** Catamorphisms
  , cataH, cataProp
  -- ** Utilities
  , mapProps, mapChildren, injectProps, eitherH
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


import           Control.Arrow                 (second)
import qualified Control.Categorical.Functor   as F
import           Control.Category              ((.))
import           Control.PseudoInverseCategory (EndoIso (..),
                                                HasHaskFunctors (fmapA),
                                                PIArrow (piendo, piiso, pisecond),
                                                PseudoInverseCategory (piinverse),
                                                ToHask (piapply))
import           Data.Functor.Identity         (Identity (Identity, runIdentity))
import           Data.Kind                     (Type)
import           Data.List                     (foldl')
import           Data.Map                      (alter, toList)
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
data Html :: (Type -> Type) -> Type -> Type where
  -- | A standard node in the DOM tree
  Node :: Text -> [(Text, Prop m a)] -> [Html m a] -> Html m a
  -- | If you can bake an element into a 'RawNode' then you can embed it as a baked potato.
  -- Backend does not provide any state management or abstraction to deal with
  -- custom embedded content; it's on you to decide how and when this 'RawNode' will
  -- be updated. For example, if you wanted to embed a Google map as a baked potato,
  -- and you are driving your Backend view with a 'TVar', you would need to build
  -- the 'RawNode' for this map /outside/ of your Backend view and pass it in
  -- as an argument. The 'RawNode' is a reference you control.
  Potato :: JSM RawNode -> Html m a
  -- | The humble text node
  TextNode :: Text -> Html m a


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


-- | Ensure all prop keys are unique.
-- Collisions for Data, Text, Flags, and Potatoes are last write wins
-- Collisions for Listeners are Continuation Semigroup operations
nubProps :: Monad m => Html m a -> Html m a
nubProps = mapPropsRecursive $ toList . foldl' f mempty
  where
  f acc (t,p) = alter (Just . g t p) t acc
  g k new old = case (new, old) of
    (PText t, Just (PText t')) | k == "className" -> PText $ t <> " " <> t'
    (PListener l, Just (PListener l')) -> PListener $
      \raw evt -> mappend <$> l raw evt <*> l' raw evt
    _ -> new


mapPropsRecursive :: ([(Text, Prop m a)] -> [(Text, Prop m a)]) -> Html m a -> Html m a
mapPropsRecursive f = \case
  Node t ps cs -> Node t (f ps) (mapPropsRecursive f <$> cs)
  x            -> x


-- | Construct a listener from its name and a simple monadic event handler.
listenM :: Monad m => Text -> m (a -> a) -> (Text, Prop m a)
listenM k = listenC k . impur


-- | Construct a listener from its name and a simple stateless monadic event handler.
listenM_ :: Monad m => Text -> m () -> (Text, Prop m a)
listenM_ k = listenC k . causes


-- | Type alias for convenience (typing out the nested brackets is tiresome)
type Props' m a = [(Text, Prop m a)]


-- | If you can provide a Natural Transformation from one Functor to another
-- then you may change the action of 'Html'.
hoistHtml :: Functor m => Functor n => (m ~> n) -> Html m a -> Html n a
hoistHtml f = \case
  Node t ps cs -> Node t (fmap (hoistProp f) <$> ps) (hoistHtml f <$> cs)
  Potato p     -> Potato p
  TextNode t   -> TextNode t
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
  fromString = TextNode . pack
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
instance Monad m => F.Functor EndoIso EndoIso (Html m) where
  map (EndoIso f g i) = EndoIso (mapC . piapply $ map' (piendo f))
                                (mapC . piapply $ map' (piiso g i))
                                (mapC . piapply $ map' (piiso i g))
    where map' :: EndoIso a b -> EndoIso (Continuation m a) (Continuation m b)
          map' = F.map
  {-# INLINE map #-}


-- | Prop is a functor in the EndoIso category, where the objects are types
--  and the morphisms are EndoIsos.
instance Monad m => F.Functor EndoIso EndoIso (Prop m) where
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
  mapC f (Node t ps es) = Node t (unMapProps . mapC f $ MapProps ps) (mapC f <$> es)
  mapC _ (Potato p) = Potato p
  mapC _ (TextNode t) = TextNode t
  {-# INLINE mapC #-}


-- | Newtype to deal with the fact that we can't make the typeclass instances
--   for Endofunctor EndoIso and Continuous using the Props type alias
newtype MapProps m a = MapProps { unMapProps :: Props' m a }


-- | Props is a functor in the EndoIso category, where the objects are
--  types and the morphisms are EndoIsos.
instance Monad m => F.Functor EndoIso EndoIso (MapProps m) where
  map f = piiso MapProps unMapProps . fmapA (pisecond (F.map f)) . piiso unMapProps MapProps
  {-# INLINE map #-}


-- | Given a lens, you can change the type of a Props by using the lens
--   to convert the types of the Continuations inside.
instance Continuous MapProps where
  mapC f = MapProps . fmap (second (mapC f)) . unMapProps
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
h = Node
{-# INLINE h #-}


-- | Construct a 'Potato' from a 'JSM' action producing a 'RawNode'.
baked :: JSM RawNode -> Html m a
baked = Potato
{-# INLINE baked #-}


-- | Construct a text node.
text :: Text -> Html m a
text = TextNode
{-# INLINE text #-}


-- | Lens to props
props :: Applicative f => ([(Text, Prop m a)] -> f [(Text, Prop m a)]) -> Html m a -> f (Html m a)
props inj = \case
  Node t ps cs -> (\ps' -> Node t ps' cs) <$> inj ps
  t            -> pure t
{-# INLINE props #-}


-- | Lens to children
children :: Applicative f => ([Html m a] -> f [Html m a]) -> Html m a -> f (Html m a)
children inj = \case
  Node t ps cs -> Node t ps <$> inj cs
  t            -> pure t
{-# INLINE children #-}


-- | Lens to tag name
name :: Applicative f => (Text -> f Text) -> Html m a -> f (Html m a)
name inj = \case
  Node t ps cs -> (\t' -> Node t' ps cs) <$> inj t
  t            -> pure t
{-# INLINE name #-}


-- | Lens to content of 'TextNode's
textContent :: Applicative f => (Text -> f Text) -> Html m a -> f (Html m a)
textContent inj = \case
  TextNode t -> TextNode <$> inj t
  n          -> pure n
{-# INLINE textContent #-}


-- | Construct an HTML element out of heterogeneous alternatives.
eitherH :: Monad m => (a -> Html m a) -> (b -> Html m b) -> Either a b -> Html m (Either a b)
eitherH = eitherC
{-# INLINE eitherH #-}


-- | Fold an HTML element, i.e. transform an h-algebra into an h-catamorphism.
cataH :: (Text -> [(Text, Prop m a)] -> [b] -> b)
      -> (JSM RawNode -> b)
      -> (Text -> b)
      -> Html m a -> b
cataH f g h' = \case
  Node t ps cs -> f t ps (cataH f g h' <$> cs)
  Potato p     -> g p
  TextNode t   -> h' t


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
mapProps f = runIdentity . props (Identity . f)
{-# INLINE mapProps #-}


-- | Transform the children of some Node. This has no effect on 'TextNode's or 'Potato'es.
mapChildren :: ([Html m a] -> [Html m a]) -> Html m a -> Html m a
mapChildren f = runIdentity . children (Identity . f)
{-# INLINE mapChildren #-}


-- | Inject props into an existing 'Node'.
injectProps :: [(Text, Prop m a)] -> Html m a -> Html m a
injectProps ps = mapProps (++ ps)
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
  -> a
  -- ^ What is the initial state?
  -> TVar a
  -- ^ How can we know when to update?
  -> (a -> Html (b m) a)
  -- ^ How should the HTML look?
  -> b m RawNode
  -- ^ Where do we render?
  -> JSM ()
shpadoinkle toJSM toM initial model view stage = do
  let
    j :: b m ~> JSM
    j = toJSM . toM model

    go :: RawNode -> VNode b m -> a -> JSM (VNode b m)
    go c n a = j $ do
      !m  <- interpret toJSM . nubProps $ view a
      patch c (Just n) m

  setup @b @m @a $ do
    (c,n) <- j $ do
      c <- stage
      n <- interpret toJSM . nubProps $ view initial
      _ <- patch c Nothing n
      return (c,n)
    _ <- shouldUpdate (go c) n model
    return ()

