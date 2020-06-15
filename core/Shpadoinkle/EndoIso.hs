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
  ( Html'(..)
  , Prop' (..)
  , mapHtml
  , mapProp
  ) where


import           Control.Arrow
import qualified Control.Categorical.Functor   as F
import           Control.Category              ((.))
import           Control.PseudoInverseCategory
import           Data.Continuation
import           Data.Kind
import           Data.String
import           Data.Text
import           Language.Javascript.JSaddle   (JSM)
import           Prelude                       hiding ((.))

import           Shpadoinkle.Class


-- | This is the core type in Backend.
-- Please note, this is NOT the Virtual DOM used by Backend.
-- This type backs a DSL that is then /interpreted/ into Virtual DOM
-- by the backend of your choosing. HTML comments are not supported.
data Html' :: (Type -> Type) -> Type -> Type where
  -- | A standard node in the dom tree
  Node' :: Text -> [(Text, Prop' m a)] -> [Html' m a] -> Html' m a
  -- | If you can bake an element into a 'RawNode' then you can embed it as a baked potato.
  -- Backend does not provide any state management or abstraction to deal with
  -- custom embedded content. It's on you to decide how and when this 'RawNode' will
  -- be updated. For example, if you wanted to embed a google map as a baked potato,
  -- and you are driving your Backend view with a 'TVar', you would need to build
  -- the 'RawNode' for this map /outside/ of your Backend view, and pass it in
  -- as an argument. The 'RawNode' is a reference you control.
  Potato' :: JSM RawNode -> Html' m a
  -- | The humble text node
  TextNode' :: Text -> Html' m a


-- | Properties of a DOM node. Backend does not use attributes directly,
-- but rather is focused on the more capable properties that may be set on a dom
-- node in JavaScript. If you wish to add attributes, you may do so
-- by setting its corresponding property.
data Prop' :: (Type -> Type) -> Type -> Type where
  -- | A text property
  PText' :: Text -> Prop' m a
  -- | Event listeners are provided with the 'RawNode' target, and the 'RawEvent', and may perform
  -- a monadic action such as a side effect. This is the one and only place where you may
  -- introduce a custom monadic action.
  PListener' :: (RawNode -> RawEvent -> JSM (Continuation m a)) -> Prop' m a
  -- | A boolean property works as a flag:
  -- for example @("disabled", PFlag' False)@ has no effect,
  -- while @("disabled", PFlag' True)@ will add the @disabled@ attribute.
  PFlag' :: Bool -> Prop' m a


-- | Type alias for convenience. Typing out the nested brackets is tiresome.
type Props' m a = [(Text, Prop' m a)]


-- | If you can provide a Natural Transformation from one Monad to another
-- then you may change the action of @Html@.
mapHtml :: Functor m => (m ~> n) -> Html' m a -> Html' n a
mapHtml f = \case
  Node' t ps cs -> Node' t (fmap (mapProp f) <$> ps) (mapHtml f <$> cs)
  Potato' p -> Potato' p
  TextNode' t -> TextNode' t
{-# INLINE mapHtml #-}


-- | If you can provide a Natural Transformation from one Monad to another
-- then you may change the action of @Prop@.
mapProp :: Functor m => (m ~> n) -> Prop' m a -> Prop' n a
mapProp f = \case
  PListener' g -> PListener' (\x y -> convertC f <$> g x y)
  PText' t     -> PText' t
  PFlag' b     -> PFlag' b
{-# INLINE mapProp #-}


-- | Strings are overloaded as HTML text nodes:
-- @
--   "hiya" = TextNode' "hiya"
-- @
instance IsString (Html' m a) where
  fromString = TextNode' . pack
  {-# INLINE fromString #-}


-- | Strings are overloaded as text props:
-- @
--   ("id", "foo") = ("id", PText' "foo")
-- @
instance IsString (Prop' m a) where
  fromString = PText' . pack
  {-# INLINE fromString #-}


-- | @Html m@ is a functor in the EndoIso category, where the objects are
--   types and the morphisms are EndoIsos.
instance Monad m => F.Functor EndoIso EndoIso (Html' m) where
  map (EndoIso f g i) = EndoIso (mapMC . piapply $ map' (piendo f))
                                (mapMC . piapply $ map' (piiso g i))
                                (mapMC . piapply $ map' (piiso i g))
    where map' :: EndoIso a b -> EndoIso (Continuation m a) (Continuation m b)
          map' = F.map
  {-# INLINE map #-}


-- | Prop is a functor in the EndoIso category (where the objects are types
--  and the morphisms are EndoIsos).
instance Monad m => F.Functor EndoIso EndoIso (Prop' m) where
  map :: forall a b. EndoIso a b -> EndoIso (Prop' m a) (Prop' m b)
  map f = EndoIso id mapFwd mapBack
    where f' :: EndoIso (Continuation m a) (Continuation m b)
          f' = F.map f

          mapFwd (PText' t)     = PText' t
          mapFwd (PListener' g) = PListener' (\r e -> piapply f' <$> g r e)
          mapFwd (PFlag' b)     = PFlag' b

          mapBack (PText' t) = PText' t
          mapBack (PListener' g) = PListener' (\r e -> piapply (piinverse f') <$> g r e)
          mapBack (PFlag' b) = PFlag' b
  {-# INLINE map #-}


-- | Given a lens, you can change the type of an Html, by using the lens
--   to convert the types of the continuations inside it.
instance MapContinuations Html' where
  mapMC f (Node' t ps es) = Node' t (unMapProps . mapMC f $ MapProps ps) (mapMC f <$> es)
  mapMC _ (Potato' p) = Potato' p
  mapMC _ (TextNode' t) = TextNode' t
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
instance MapContinuations Prop' where
  mapMC _ (PText' t)     = PText' t
  mapMC f (PListener' g) = PListener' (\r e -> f <$> g r e)
  mapMC _ (PFlag' b)     = PFlag' b
  {-# INLINE mapMC #-}


instance IsProp (Prop' m) (Continuation m) where
  textProp = PText'
  {-# INLINE textProp #-}
  listenerProp f = PListener' (\r e -> f r e)
  {-# INLINE listenerProp #-}
  flagProp = PFlag'
  {-# INLINE flagProp #-}


instance Monad m => IsHtml (Html' m) (Prop' m) where
  h = Node'
  {-# INLINE h #-}
  baked = Potato'
  {-# INLINE baked #-}
  text = TextNode'
  {-# INLINE text #-}
  props inj = \case
    Node' t ps cs -> (\ps' -> Node' t ps' cs) <$> inj ps
    t -> pure t
  {-# INLINE props #-}
  children inj = \case
    Node' t ps cs -> Node' t ps <$> inj cs
    t -> pure t
  {-# INLINE children #-}
  name inj = \case
    Node' t ps cs -> (\t' -> Node' t' ps cs) <$> inj t
    t -> pure t
  {-# INLINE name #-}
  textContent inj = \case
    TextNode' t -> TextNode' <$> inj t
    n -> pure n
  {-# INLINE textContent #-}
  eitherH = eitherMC
  {-# INLINE eitherH #-}
