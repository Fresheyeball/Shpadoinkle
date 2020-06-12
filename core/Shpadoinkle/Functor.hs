{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Shpadoinkle.Functor where

import Control.Arrow
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import Shpadoinkle

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

data Prop' o =
    PText' Text
  | PListener' (RawNode -> RawEvent -> o)
  | PFlag' Bool

instance Functor Prop' where
  fmap _ (PText' t) = PText' t
  fmap f (PListener' g) = PListener' (\r e -> f (g r e))
  fmap _ (PFlag' b) = PFlag' b

constly :: Applicative m => (a -> b -> b) -> Html' a -> Html m b
constly f (Node' t ps es) = Node t (second (constlyProp f) <$> ps) (fmap (constly f) es)
constly _ (Potato' p) = Potato p
constly _ (TextNode' t) = TextNode t

constlyProp :: Applicative m => (a -> b -> b) -> Prop' a -> Prop m b
constlyProp _ (PText' t) = PText t
constlyProp f (PListener' g) = PListener (\r e -> pure . pur . f $ g r e)
constlyProp _ (PFlag' b) = PFlag b

class Propish p e | p -> e where
  propText :: Text -> p o
  propListener :: (RawNode -> RawEvent -> e o) -> p o
  propFlag :: Bool -> p o

instance Propish Prop' Identity where
  propText = PText'
  propListener f = PListener' (\r e -> runIdentity (f r e))
  propFlag = PFlag'

newtype EventHandler m o = EventHandler { runEventHandler :: (m (Continuation m o)) }

instance Propish (Prop m) (EventHandler m) where
  propText = PText
  propListener f = PListener (\r e -> runEventHandler (f r e))
  propFlag = PFlag

class Htmlish h p | h -> p where
  htmlNode :: Text -> [(Text, p o)] -> [h o] -> h o
  htmlPotato :: JSM RawNode -> h o
  htmlText :: Text -> h o

instance Htmlish Html' Prop' where
  htmlNode = Node'
  htmlPotato = Potato'
  htmlText = TextNode'

instance Htmlish (Html m) (Prop m) where
  htmlNode = Node
  htmlPotato = Potato
  htmlText = TextNode
