{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}


module Shpadoinkle.Functor
  ( Html (..), Prop (..), Props
  ) where


import           Control.Arrow
import           Data.Functor.Identity       (Identity (..))
import           Data.Kind
import           Data.String
import           Data.Text                   (Text, pack)
import           Language.Javascript.JSaddle (JSM)

import           Shpadoinkle.Class


-- | @HtmlM m@ and @PropM m@ are not Functors because @Continuation@ is
--   not a @Functor@. @Continuation@ is not a @Functor@ fundamentally because
--   @a -> a@ is not a @Functor@; there is no (total computable) function
--   @(a -> b) -> ((a -> a) -> (b -> b))@.
--
--   @Html@ is a @Functor@ with the property that @Html a@ can be turned
--   into @HtmlM m b@ via a mapping function of type @a -> b -> b@.
--   constly is the function which performs this change. Since Html'
--   is a Functor, it can be more convenient to work with than Html m.
--
--   The major limitations of @Html@ are:
--     1. Event handlers cannot have side effects.
--     2. The return value of an event handler cannot depend on the current
--        state, but only on the properties of the event and target node.
--
--   The limitations are there to allow @Html@ to be a @Functor@ which can be
--   used with correct semantics even in concurrent apps. @Html@ is only
--   correct to use when the event handlers in the HTML satisfy conditions
--   1 and 2 in a semantic sense. It is technically possible to write @Html@
--   where the event handlers return values based on a past state passed
--   in via a closure (which may or may not be the current state).
--   However, it is not semantically correct to do so. An example of a
--   semantically correct usage of @Html@ is to create a text field
--   of type @Html Text@ where the value is whatever the user typed.
--   Another example of a semantically correct usage of @Html@
--   is to create a number pad widget of type @Html Int@ where each
--   number button's click handler returns the number it is labeled with.
data Html :: Type -> Type where
  Node :: Text -> Props a -> [Html a] -> Html a
  Potato :: JSM RawNode -> Html a
  TextNode :: Text -> Html a


instance Functor Html where
  fmap f (Node t ps es) = Node t (second (fmap f) <$> ps) (fmap f <$> es)
  fmap _ (Potato p)     = Potato p
  fmap _ (TextNode t)   = TextNode t


data Prop :: Type -> Type where
  PText :: Text -> Prop a
  PListener :: (RawNode -> RawEvent -> JSM a) -> Prop a
  PFlag :: Bool -> Prop a


instance Functor Prop where
  fmap _ (PText t)     = PText t
  fmap f (PListener g) = PListener (\r e -> f <$> g r e)
  fmap _ (PFlag b)     = PFlag b


type Props a = [(Text, Prop a)]


instance IsProp Prop Identity where
  textProp = PText
  {-# INLINE textProp #-}
  listenerProp f = PListener (\r e -> runIdentity <$> f r e)
  {-# INLINE listenerProp #-}
  flagProp = PFlag
  {-# INLINE flagProp #-}
  constUpdate _ = Identity
  {-# INLINE constUpdate #-}
  cataProp f g h' = \case
    PText t -> f t
    PListener l -> g (\n e -> Identity <$> l n e)
    PFlag b -> h' b
  {-# INLINE cataProp #-}


instance IsHtml Html Prop where
  h = Node
  {-# INLINE h #-}
  baked = Potato
  {-# INLINE baked #-}
  text = TextNode
  {-# INLINE text #-}
  props inj = \case
    Node t ps cs -> (\ps' -> Node t ps' cs) <$> inj ps
    t -> pure t
  {-# INLINE props #-}
  children inj = \case
    Node t ps cs -> Node t ps <$> inj cs
    t -> pure t
  {-# INLINE children #-}
  name inj = \case
    Node t ps cs -> (\t' -> Node t' ps cs) <$> inj t
    t -> pure t
  {-# INLINE name #-}
  textContent inj = \case
    TextNode t -> TextNode <$> inj t
    n -> pure n
  {-# INLINE textContent #-}
  eitherH l r = either (fmap Left . l) (fmap Right . r)
  {-# INLINE eitherH #-}
  cataH f g h' = \case
    Node t ps cs -> f t ps (cataH f g h' <$> cs)
    Potato p -> g p
    TextNode t -> h' t


instance IsString (Html a) where
  fromString = text . pack
  {-# INLINE fromString #-}


instance IsString (Prop a) where
  fromString = textProp . pack
  {-# INLINE fromString #-}
