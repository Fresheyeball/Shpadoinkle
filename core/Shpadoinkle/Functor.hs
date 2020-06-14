{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}

module Shpadoinkle.Functor
  ( Html' (..), Prop' (..), Constly (..)
  , Propish (..), Htmlish (..)
  , listener, listenRaw, listen, listen'
  , mapProps, mapChildren, injectProps
  ) where

import           Control.Arrow
import           Data.Functor.Identity (Identity (..))
import           Data.String
import           Data.Text (Text, pack)
import           Shpadoinkle.Core
import           Shpadoinkle.Continuation

-- | `Html m` and `Prop m` are not Functors because Continuation is
--   not a Functor. Continuation is not a Functor fundamentally because
--   (a -> a) is not a Functor; there is no (total computable) function
--   (a -> b) -> ((a -> a) -> (b -> b)).
--
--   Html' is a Functor with the property that Html' a can be turned
--   into Html m b via a mapping function of type a -> b -> b.
--   constly is the function which performs this change. Since Html'
--   is a Functor, it can be more convenient to work with than Html m.
--
--   The major limitations of Html' are:
--     1. Event handlers cannot have side effects.
--     2. The return value of an event handler cannot depend on the current
--        state, but only on the properties of the event and target node.
--
--   The limitations are there to allow Html' to be a Functor which can be
--   used with correct semantics even in concurrent apps. Html' is only
--   correct to use when the event handlers in the HTML satisfy conditions
--   1 and 2 in a semantic sense. It is technically possible to write Html'
--   where the event handlers return values based on a past state passed
--   in via a closure (which may or may not be the current state).
--   However, it is not semantically correct to do so. An example of a
--   semantically correct usage of Html' is to create a text field
--   of type Html' Text where the value is whatever the user typed.
--   Another example of a semantically correct usage of Html'
--   is to create a number pad widget of type Html' Int where each
--   number button's click handler returns the number it is labeled with.
data Html' o =
    Node' Text [(Text, Prop' o)] [Html' o]
  | Potato' (JSM RawNode)
  | TextNode' Text


instance Functor Html' where
  fmap f (Node' t ps es) = Node' t (second (fmap f) <$> ps) (fmap f <$> es)
  fmap _ (Potato' p) = Potato' p
  fmap _ (TextNode' t) = TextNode' t


data Prop' o where
  PText' :: Text -> Prop' o
  PListener' :: (RawNode -> RawEvent -> JSM o) -> Prop' o
  PFlag' :: Bool -> Prop' o


instance Functor Prop' where
  fmap _ (PText' t) = PText' t
  fmap f (PListener' g) = PListener' (\r e -> f <$> g r e)
  fmap _ (PFlag' b) = PFlag' b


class Constly f g where
  constly :: (a -> b -> b) -> f a -> g b


instance Applicative m => Constly Html' (Html m) where
  constly f (Node' t ps es) = Node t (second (constly f) <$> ps) (fmap (constly f) es)
  constly _ (Potato' p) = Potato p
  constly _ (TextNode' t) = TextNode t


instance Applicative m => Constly Prop' (Prop m) where
  constly _ (PText' t) = PText t
  constly f (PListener' g) = PListener (\r e -> pur . f <$> g r e)
  constly _ (PFlag' b) = PFlag b


class Propish p e | p -> e where
  textProp :: Text -> p o
  listenerProp :: (RawNode -> RawEvent -> JSM (e o)) -> p o
  flagProp :: Bool -> p o


instance Propish Prop' Identity where
  textProp = PText'
  listenerProp f = PListener' (\r e -> runIdentity <$> f r e)
  flagProp = PFlag'


instance Propish (Prop m) (Continuation m) where
  textProp = PText
  listenerProp f = PListener (\r e -> f r e)
  flagProp = PFlag


class Htmlish h p | h -> p where
  -- | JSX style HTML element constructor
  h :: Text -> [(Text, p o)] -> [h o] -> h o
  -- | Construct a 'Potato' from a 'JSM' action producing a 'RawNode'
  baked :: JSM RawNode -> h o
  -- | Construct a text node
  text :: Text -> h o
  -- | Lens to props
  props :: Applicative f => ([(Text, p o)] -> f [(Text, p o)]) -> h o -> f (h o)
  -- | Lens to children
  children :: Applicative f => ([h o] -> f [h o]) -> h o -> f (h o)
  -- | Lens to tag name
  name :: Applicative f => (Text -> f Text) -> h o -> f (h o)
  -- | Lens to content of @TextNode@s
  textContent :: Applicative f => (Text -> f Text) -> h o -> f (h o)


instance Htmlish Html' Prop' where
  h = Node'
  baked = Potato'
  text = TextNode'
  props inj = \case
    Node' t ps cs -> (\ps' -> Node' t ps' cs) <$> inj ps
    t -> pure t
  children inj = \case
    Node' t ps cs -> Node' t ps <$> inj cs
    t -> pure t
  name inj = \case
    Node' t ps cs -> (\t' -> Node' t' ps cs) <$> inj t
    t -> pure t
  textContent inj = \case
    TextNode' t -> TextNode' <$> inj t
    n -> pure n


instance Htmlish (Html m) (Prop m) where
  h = Node
  baked = Potato
  text = TextNode
  props inj = \case
    Node t ps cs -> (\ps' -> Node t ps' cs) <$> inj ps
    t -> pure t
  children inj = \case
    Node t ps cs -> Node t ps <$> inj cs
    t -> pure t
  name inj = \case
    Node t ps cs -> (\t' -> Node t' ps cs) <$> inj t
    t -> pure t
  textContent inj = \case
    TextNode t -> TextNode <$> inj t
    n -> pure n


instance IsString (Html' o) where
  fromString = text . pack
  {-# INLINE fromString #-}


instance IsString (Prop' o) where
  fromString = textProp . pack
  {-# INLINE fromString #-}


instance {-# OVERLAPPING #-} IsString [(Text, Prop' o)] where
  fromString = pure . ("className", ) . textProp . pack


-- | Construct a simple listener property that will perform an action.
listener :: Propish p e => e o -> p o
listener = listenerProp . const . const . return
{-# INLINE listener #-}


-- | Construct a listener from its name and an event handler.
listenRaw :: Propish p e => Text -> (RawNode -> RawEvent -> JSM (e o)) -> (Text, p o)
listenRaw k = (,) k . listenerProp
{-# INLINE listenRaw #-}


-- | Construct a listener from its name and an event handler.
listen :: Propish p e => Text -> e o -> (Text, p o)
listen k = listenRaw k . const . const . return
{-# INLINE listen #-}


-- | Construct a listener from it's 'Text' name and an output value.
listen' :: Propish p Identity => Text -> o -> (Text, p o)
listen' k f = listen k $ pure f


-- | Transform the properties of some Node. This has no effect on @TextNode@s or @Potato@s
mapProps :: Htmlish h p => ([(Text, p o)] -> [(Text, p o)]) -> h o -> h o
mapProps f = runIdentity . props (Identity . f)


-- | Transform the children of some Node. This has no effect on @TextNode@s or @Potato@s
mapChildren :: Htmlish h p => ([h a] -> [h a]) -> h a -> h a
mapChildren f = runIdentity . children (Identity . f)


-- | Inject props into an existing @Node@
injectProps :: Htmlish h p => [(Text, p o)] -> h o -> h o
injectProps ps = mapProps (++ ps)
