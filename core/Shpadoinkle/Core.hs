{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


{-|
   I think I know precisely what I mean.

   Shpadoinkle is an abstract frontend programming model, with one way data flow, and a single source of truth.
   This module provides a parsimonious implementation of Shpadoinkle with few implementation details.
-}


module Shpadoinkle.Core (
  -- * Base Types
  Html(..), Prop(..)
  -- ** Prop Constructors
  , textProp, listenerProp, flagProp
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
  -- ** Convenience Variants
  , runJSorWarp
  , fullPage
  , fullPageJSM
  , simple
  -- * Re-Exports
  , JSM, MonadJSM, TVar, newTVarIO, readTVarIO
  ) where


import           Control.Arrow                    (second)
import qualified Control.Categorical.Functor      as F
import           Control.Category                 ((.))
import           Control.PseudoInverseCategory
import           Data.Functor.Identity
import           Data.Kind
import           Data.String
import           Data.Text
import           Language.Javascript.JSaddle      (FromJSVal (..), JSM, JSVal,
                                                   MonadJSM, ToJSVal (..))
import           Prelude                          hiding ((.))
import           UnliftIO.STM                     (TVar, newTVarIO, readTVarIO)
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp (run)
#endif


import           Shpadoinkle.Continuation


-- | This is the core type in Backend.
-- Please note, this is NOT the Virtual DOM used by Backend.
-- This type backs a DSL that is then /interpreted/ into Virtual DOM
-- by the backend of your choosing. HTML comments are not supported.
data Html :: (Type -> Type) -> Type -> Type where
  -- | A standard node in the dom tree
  Node :: Text -> [(Text, Prop m a)] -> [Html m a] -> Html m a
  -- | If you can bake an element into a 'RawNode' then you can embed it as a baked potato.
  -- Backend does not provide any state management or abstraction to deal with
  -- custom embedded content. It's on you to decide how and when this 'RawNode' will
  -- be updated. For example, if you wanted to embed a google map as a baked potato,
  -- and you are driving your Backend view witHtml m a 'TVar', you would need to build
  -- the 'RawNode' for this map /outside/ of your Backend view, and pass it in
  -- as an argument. The 'RawNode' is a reference you control.
  Potato :: JSM RawNode -> Html m a
  -- | The humble text node
  TextNode :: Text -> Html m a


-- | Properties of a DOM node. Backend does not use attributes directly,
-- but rather is focused on the more capable properties that may be set on a dom
-- node in JavaScript. If you wish to add attributes, you may do so
-- by setting its corresponding property.
data Prop :: (Type -> Type) -> Type -> Type where
  -- | A text property
  PText :: Text -> Prop m a
  -- | Event listeners are provided with the 'RawNode' target, and the 'RawEvent', and may perform
  -- a monadic action sucHtml m as a side effect. This is the one and only place where you may
  -- introduce a custom monadic action. The JSM to compute the Continuation must be
  -- synchronous and non-blocking; otherwise race conditions may result from a Pure
  -- Continuation which sets the state based on a previous state captured by the closure.
  -- Such continuations must be executed synchronously during event bubbling,
  -- and that may not be the case if the code to compute the Continuation of some
  -- listener is blocking.
  PListener :: (RawNode -> RawEvent -> JSM (Continuation m a)) -> Prop m a
  -- | A boolean property works as a flag:
  -- for example @("disabled", PFlag False)@ has no effect,
  -- while @("disabled", PFlag True)@ will add the @disabled@ attribute.
  PFlag :: Bool -> Prop m a


-- | Construct a listener from its name and a simple monadic event handler.
listenM :: Monad m => Text -> m (a -> a) -> (Text, Prop m a)
listenM k = listenC k . impur


-- | Construct a listener from its name and a simple stateless monadic event handler.
listenM_ :: Monad m => Text -> m () -> (Text, Prop m a)
listenM_ k = listenC k . causes


-- | Type alias for convenience. Typing out the nested brackets is tiresome.
type Props' m a = [(Text, Prop m a)]


-- | If you can provide a Natural Transformation from one Functor to another
-- then you may change the action of 'Html'.
hoistHtml :: Functor m => (m ~> n) -> Html m a -> Html n a
hoistHtml f = \case
  Node t ps cs -> Node t (fmap (hoistProp f) <$> ps) (hoistHtml f <$> cs)
  Potato p -> Potato p
  TextNode t -> TextNode t
{-# INLINE hoistHtml #-}


-- | If you can provide a Natural Transformation from one Functor to another
-- then you may change the action of 'Prop'.
hoistProp :: Functor m => (m ~> n) -> Prop m a -> Prop n a
hoistProp f = \case
  PListener g -> PListener (\x y -> hoist f <$> g x y)
  PText t     -> PText t
  PFlag b     -> PFlag b
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


-- | Prop is a functor in the EndoIso category (where the objects are types
--  and the morphisms are EndoIsos).
instance Monad m => F.Functor EndoIso EndoIso (Prop m) where
  map :: forall a b. EndoIso a b -> EndoIso (Prop m a) (Prop m b)
  map f = EndoIso id mapFwd mapBack
    where f' :: EndoIso (Continuation m a) (Continuation m b)
          f' = F.map f

          mapFwd (PText t)     = PText t
          mapFwd (PListener g) = PListener (\r e -> piapply f' <$> g r e)
          mapFwd (PFlag b)     = PFlag b

          mapBack (PText t) = PText t
          mapBack (PListener g) = PListener (\r e -> piapply (piinverse f') <$> g r e)
          mapBack (PFlag b) = PFlag b
  {-# INLINE map #-}


-- | Given a lens, you can change the type of an Html, by using the lens
--   to convert the types of the continuations inside it.
instance Continuous Html where
  mapC f (Node t ps es) = Node t (unMapProps . mapC f $ MapProps ps) (mapC f <$> es)
  mapC _ (Potato p) = Potato p
  mapC _ (TextNode t) = TextNode t
  {-# INLINE mapC #-}


-- | Newtype to deal with the fact that we can't make the typeclass instances
--   for Endofunctor EndoIso and Continuous using the Props type alias.
newtype MapProps m a = MapProps { unMapProps :: Props' m a }


-- | Props is a functor in the EndoIso category (where the objects are
--  types and the morphisms are EndoIsos).
instance Monad m => F.Functor EndoIso EndoIso (MapProps m) where
  map f = piiso MapProps unMapProps . fmapA (pisecond (F.map f)) . piiso unMapProps MapProps
  {-# INLINE map #-}


-- | Given a lens, you can change the type of a Props, by using the lens
--   to convert the types of the continuations inside.
instance Continuous MapProps where
  mapC f = MapProps . fmap (second (mapC f)) . unMapProps
  {-# INLINE mapC #-}


-- | Given a lens, you can change the type of a Prop, by using the
--   lens to convert the types of the continuations which it contains
--   if it is a listener.
instance Continuous Prop where
  mapC _ (PText t)     = PText t
  mapC f (PListener g) = PListener (\r e -> f <$> g r e)
  mapC _ (PFlag b)     = PFlag b
  {-# INLINE mapC #-}


-- | Create a text property.
textProp :: Text -> Prop m a
textProp = PText
{-# INLINE textProp #-}


-- | Create an event listener property.
listenerProp :: (RawNode -> RawEvent -> JSM (Continuation m a)) -> Prop m a
listenerProp f = PListener (\r e -> f r e)
{-# INLINE listenerProp #-}


-- | Create a boolean property.
flagProp :: Bool -> Prop m a
flagProp = PFlag
{-# INLINE flagProp #-}


-- | Transform a p-algebra into a p-catamorphism. This is like polymorphic pattern matching.
cataProp :: (Text -> b)
         -> ((RawNode -> RawEvent -> JSM (Continuation m a)) -> b)
         -> (Bool -> b)
         -> Prop m a -> b
cataProp f g h' = \case
  PText t -> f t
  PListener l -> g l
  PFlag b -> h' b


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
  t -> pure t
{-# INLINE props #-}


-- | Lens to children
children :: Applicative f => ([Html m a] -> f [Html m a]) -> Html m a -> f (Html m a)
children inj = \case
  Node t ps cs -> Node t ps <$> inj cs
  t -> pure t
{-# INLINE children #-}


-- | Lens to tag name
name :: Applicative f => (Text -> f Text) -> Html m a -> f (Html m a)
name inj = \case
  Node t ps cs -> (\t' -> Node t' ps cs) <$> inj t
  t -> pure t
{-# INLINE name #-}


-- | Lens to content of 'TextNode's
textContent :: Applicative f => (Text -> f Text) -> Html m a -> f (Html m a)
textContent inj = \case
  TextNode t -> TextNode <$> inj t
  n -> pure n
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
  Potato p -> g p
  TextNode t -> h' t


-- | Natural Transformation
type m ~> n = forall a. m a -> n a


-- | A DOM node reference.
-- Useful for building baked potatoes, and binding a Backend view to the page
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


-- | Construct a listener from it's 'Text' name and an output value.
listen :: Text -> a -> (Text, Prop m a)
listen k = listenC k . constUpdate
{-# INLINE listen #-}


-- | Transform the properties of some Node. This has no effect on 'TextNode's or 'Potato's
mapProps :: ([(Text, Prop m a)] -> [(Text, Prop m a)]) -> Html m a -> Html m a
mapProps f = runIdentity . props (Identity . f)
{-# INLINE mapProps #-}


-- | Transform the children of some Node. This has no effect on 'TextNode's or 'Potato's
mapChildren :: ([Html m a] -> [Html m a]) -> Html m a -> Html m a
mapChildren f = runIdentity . children (Identity . f)
{-# INLINE mapChildren #-}


-- | Inject props into an existing 'Node'
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
  -- | A backend must be able to interpret 'Html' into its own internal Virtual DOM
  interpret
    :: (m ~> JSM)
    -- ^ Natural transformation for some @m@ to 'JSM'.
    -- This is how a Backend gets access to 'JSM' to perform the rendering side effects.
    -> Html (b m) a
    -- ^ 'Html' to interpret
    -> b m (VNode b m)
    -- ^ Effect producing the Virtual DOM representation

  -- | A backend must be able to patch the 'RawNode' containing the view, with a
  -- new view if the Virtual DOM changed.
  patch
    :: RawNode
    -- ^ The container for rendering the Backend view.
    -> Maybe (VNode b m)
    -- ^ Perhaps there is a previous Virtual DOM to diff against. Will be 'Nothing' on the first run.
    -> VNode b m
    -- ^ New Virtual DOM to render.
    -> b m (VNode b m)
    -- ^ Effect producing an updated Virtual DOM. This is not needed by all backends.
    -- Some JavaScript based backends need to do this for the next tick. Regardless, whatever
    -- 'VNode' the effect produces will be passed as the previous Virtual DOM on the next render.

  -- | A backend may perform some imperative setup steps
  setup :: JSM () -> JSM ()


-- | The core view instantiation function.
-- This combines a backend, a territory, and a model
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
      !m  <- interpret toJSM (view a)
      patch c (Just n) m

  setup @b @m @a $ do
    (c,n) <- j $ do
      c <- stage
      n <- interpret toJSM (view initial)
      _ <- patch c Nothing n
      return (c,n)
    _ <- shouldUpdate (go c) n model
    return ()


-- | Wrapper around 'shpadoinkle' for full page apps
-- that do not need outside control of the territory
fullPage
  :: Backend b m a => Monad (b m) => Eq a
  => (m ~> JSM)
  -- ^ How do we get to JSM?
  -> (TVar a -> b m ~> m)
  -- ^ What backend are we running?
  -> a
  -- ^ What is the initial state?
  -> (a -> Html (b m) a)
  -- ^ How should the html look?
  -> b m RawNode
  -- ^ Where do we render?
  -> JSM ()
fullPage g f i view getStage = do
  model <- newTVarIO i
  shpadoinkle g f i model view getStage
{-# INLINE fullPage #-}


-- | Wrapper around 'shpadoinkle' for full page apps
-- that do not need outside control of the territory
-- where actions are performed directly in JSM.
--
-- This set of assumptions is extremely common when starting
-- a new project.
fullPageJSM
  :: Backend b JSM a => Monad (b JSM) => Eq a
  => (TVar a -> b JSM ~> JSM)
  -- ^ What backend are we running?
  -> a
  -- ^ What is the initial state?
  -> (a -> Html (b JSM) a)
  -- ^ How should the html look?
  -> b JSM RawNode
  -- ^ Where do we render?
  -> JSM ()
fullPageJSM = fullPage id
{-# INLINE fullPageJSM #-}


-- | Start the program!
--
-- For GHC or GHCjs. I saved you from using CPP directly. You're welcome.
runJSorWarp :: Int -> JSM () -> IO ()
#ifdef ghcjs_HOST_OS
runJSorWarp _ = id
{-# INLINE runJSorWarp #-}
#else
runJSorWarp = run
{-# INLINE runJSorWarp #-}
#endif


-- | Simple app
--
-- A good starting place
simple
  :: Backend b JSM a => Monad (b JSM) => Eq a
  => (TVar a -> b JSM ~> JSM)
  -- ^ What backend are we running?
  -> a
  -- ^ what is the initial state?
  -> (a -> Html (b JSM) a)
  -- ^ how should the html look?
  -> b JSM RawNode
  -- ^ where do we render?
  -> JSM ()
simple = fullPageJSM
{-# INLINE simple #-}
