{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


module Shpadoinkle.EndoIso
  ( HtmlM(..)
  , PropM (..), listenM, listenM_
  , mapHtml
  , mapProp
  ) where


import           Control.Arrow
import qualified Control.Categorical.Functor     as F
import           Control.Category                ((.))
import           Control.PseudoInverseCategory
import           Control.ShpadoinkleContinuation
import           Data.Kind
import           Data.String
import           Data.Text
import           Language.Javascript.JSaddle     (JSM)
import           Prelude                         hiding ((.))

import           Shpadoinkle.Class


-- | This is the core type in Backend.
-- Please note, this is NOT the Virtual DOM used by Backend.
-- This type backs a DSL that is then /interpreted/ into Virtual DOM
-- by the backend of your choosing. HTML comments are not supported.
data HtmlM :: (Type -> Type) -> Type -> Type where
  -- | A standard node in the dom tree
  NodeM :: Text -> [(Text, PropM m a)] -> [HtmlM m a] -> HtmlM m a
  -- | If you can bake an element into a 'RawNode' then you can embed it as a baked potato.
  -- Backend does not provide any state management or abstraction to deal with
  -- custom embedded content. It's on you to decide how and when this 'RawNode' will
  -- be updated. For example, if you wanted to embed a google map as a baked potato,
  -- and you are driving your Backend view with a 'TVar', you would need to build
  -- the 'RawNode' for this map /outside/ of your Backend view, and pass it in
  -- as an argument. The 'RawNode' is a reference you control.
  PotatoM :: JSM RawNode -> HtmlM m a
  -- | The humble text node
  TextNodeM :: Text -> HtmlM m a


-- | Properties of a DOM node. Backend does not use attributes directly,
-- but rather is focused on the more capable properties that may be set on a dom
-- node in JavaScript. If you wish to add attributes, you may do so
-- by setting its corresponding property.
data PropM :: (Type -> Type) -> Type -> Type where
  -- | A text property
  PTextM :: Text -> PropM m a
  -- | Event listeners are provided with the 'RawNode' target, and the 'RawEvent', and may perform
  -- a monadic action such as a side effect. This is the one and only place where you may
  -- introduce a custom monadic action. The JSM to compute the Continuation must be
  -- synchronous and non-blocking; otherwise race conditions may result from a Pure
  -- Continuation which sets the state based on a previous state captured by the closure.
  -- Such continuations must be executed synchronously during event bubbling,
  -- and that may not be the case if the code to compute the Continuation of some
  -- listener is blocking.
  PListenerM :: (RawNode -> RawEvent -> JSM (Continuation m a)) -> PropM m a
  -- | A boolean property works as a flag:
  -- for example @("disabled", PFlagM False)@ has no effect,
  -- while @("disabled", PFlagM True)@ will add the @disabled@ attribute.
  PFlagM :: Bool -> PropM m a


-- | Construct a listener from its name and a simple monadic event handler.
listenM :: Monad m => Text -> m (a -> a) -> (Text, PropM m a)
listenM k = listenE k . impur


-- | Construct a listener from its name and a simple stateless monadic event handler.
listenM_ :: Monad m => Text -> m () -> (Text, PropM m a)
listenM_ k = listenE k . causes


-- | Type alias for convenience. Typing out the nested brackets is tiresome.
type Props' m a = [(Text, PropM m a)]


-- | If you can provide a Natural Transformation from one Monad to another
-- then you may change the action of @Html@.
mapHtml :: Functor m => (m ~> n) -> HtmlM m a -> HtmlM n a
mapHtml f = \case
  NodeM t ps cs -> NodeM t (fmap (mapProp f) <$> ps) (mapHtml f <$> cs)
  PotatoM p -> PotatoM p
  TextNodeM t -> TextNodeM t
{-# INLINE mapHtml #-}


-- | If you can provide a Natural Transformation from one Monad to another
-- then you may change the action of @Prop@.
mapProp :: Functor m => (m ~> n) -> PropM m a -> PropM n a
mapProp f = \case
  PListenerM g -> PListenerM (\x y -> convertC f <$> g x y)
  PTextM t     -> PTextM t
  PFlagM b     -> PFlagM b
{-# INLINE mapProp #-}


-- | Strings are overloaded as HTML text nodes:
-- @
--   "hiya" = TextNodeM "hiya"
-- @
instance IsString (HtmlM m a) where
  fromString = TextNodeM . pack
  {-# INLINE fromString #-}


-- | Strings are overloaded as text props:
-- @
--   ("id", "foo") = ("id", PTextM "foo")
-- @
instance IsString (PropM m a) where
  fromString = PTextM . pack
  {-# INLINE fromString #-}


-- | @HtmlM m@ is a functor in the EndoIso category, where the objects are
--   types and the morphisms are EndoIsos.
instance Monad m => F.Functor EndoIso EndoIso (HtmlM m) where
  map (EndoIso f g i) = EndoIso (mapMC . piapply $ map' (piendo f))
                                (mapMC . piapply $ map' (piiso g i))
                                (mapMC . piapply $ map' (piiso i g))
    where map' :: EndoIso a b -> EndoIso (Continuation m a) (Continuation m b)
          map' = F.map
  {-# INLINE map #-}


-- | Prop is a functor in the EndoIso category (where the objects are types
--  and the morphisms are EndoIsos).
instance Monad m => F.Functor EndoIso EndoIso (PropM m) where
  map :: forall a b. EndoIso a b -> EndoIso (PropM m a) (PropM m b)
  map f = EndoIso id mapFwd mapBack
    where f' :: EndoIso (Continuation m a) (Continuation m b)
          f' = F.map f

          mapFwd (PTextM t)     = PTextM t
          mapFwd (PListenerM g) = PListenerM (\r e -> piapply f' <$> g r e)
          mapFwd (PFlagM b)     = PFlagM b

          mapBack (PTextM t) = PTextM t
          mapBack (PListenerM g) = PListenerM (\r e -> piapply (piinverse f') <$> g r e)
          mapBack (PFlagM b) = PFlagM b
  {-# INLINE map #-}


-- | Given a lens, you can change the type of an Html, by using the lens
--   to convert the types of the continuations inside it.
instance MapContinuations HtmlM where
  mapMC f (NodeM t ps es) = NodeM t (unMapProps . mapMC f $ MapProps ps) (mapMC f <$> es)
  mapMC _ (PotatoM p) = PotatoM p
  mapMC _ (TextNodeM t) = TextNodeM t
  {-# INLINE mapMC #-}


-- | Newtype to deal with the fact that we can't make the typeclass instances
--   for Endofunctor EndoIso and MapContinuations using the Props type alias.
newtype MapProps m a = MapProps { unMapProps :: Props' m a }


-- | Props is a functor in the EndoIso category (where the objects are
--  types and the morphisms are EndoIsos).
instance Monad m => F.Functor EndoIso EndoIso (MapProps m) where
  map f = piiso MapProps unMapProps . fmapA (pisecond (F.map f)) . piiso unMapProps MapProps
  {-# INLINE map #-}


-- | Given a lens, you can change the type of a Props, by using the lens
--   to convert the types of the continuations inside.
instance MapContinuations MapProps where
  mapMC f = MapProps . fmap (second (mapMC f)) . unMapProps
  {-# INLINE mapMC #-}


-- | Given a lens, you can change the type of a Prop, by using the
--   lens to convert the types of the continuations which it contains
--   if it is a listener.
instance MapContinuations PropM where
  mapMC _ (PTextM t)     = PTextM t
  mapMC f (PListenerM g) = PListenerM (\r e -> f <$> g r e)
  mapMC _ (PFlagM b)     = PFlagM b
  {-# INLINE mapMC #-}


instance IsProp (PropM m) (Continuation m) where
  textProp = PTextM
  {-# INLINE textProp #-}
  listenerProp f = PListenerM (\r e -> f r e)
  {-# INLINE listenerProp #-}
  flagProp = PFlagM
  {-# INLINE flagProp #-}
  constUpdate _ = pur . const
  {-# INLINE constUpdate #-}
  cataProp f g h' = \case
    PTextM t -> f t
    PListenerM l -> g l
    PFlagM b -> h' b


instance Monad m => IsHtml (HtmlM m) (PropM m) where
  h = NodeM
  {-# INLINE h #-}
  baked = PotatoM
  {-# INLINE baked #-}
  text = TextNodeM
  {-# INLINE text #-}
  props inj = \case
    NodeM t ps cs -> (\ps' -> NodeM t ps' cs) <$> inj ps
    t -> pure t
  {-# INLINE props #-}
  children inj = \case
    NodeM t ps cs -> NodeM t ps <$> inj cs
    t -> pure t
  {-# INLINE children #-}
  name inj = \case
    NodeM t ps cs -> (\t' -> NodeM t' ps cs) <$> inj t
    t -> pure t
  {-# INLINE name #-}
  textContent inj = \case
    TextNodeM t -> TextNodeM <$> inj t
    n -> pure n
  {-# INLINE textContent #-}
  eitherH = eitherMC
  {-# INLINE eitherH #-}
  cataH f g h' = \case
    NodeM t ps cs -> f t ps (cataH f g h' <$> cs)
    PotatoM p -> g p
    TextNodeM t -> h' t
