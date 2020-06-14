{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
   I think I know precisely what I mean.

   A frontend abstraction motivated by simplicity, performance, and ergonomics.
   This module provides core abstractions and types with almost no implementation details. I.e., no batteries included.
   You may use this model a la carte, build on top of it, or include more Backend packages for additional batteries.

   Backend is focused on letting you build your frontend the way you want to. It's as unopinionated as possible, beyond providing a concrete programming model.
-}

module Shpadoinkle.Core
  ( Html (..), Prop (..), Props, MapProps (..)
  , mapHtml, mapProp
  , Backend (..)
  , shpadoinkle, fullPage, fullPageJSM, simple
  , type (~>)
  , RawNode (..), RawEvent (..)
  , MonadJSM, JSM, liftJSM
  , TVar, newTVarIO, readTVarIO
  , runJSorWarp
  , runJSM, askJSM
  ) where


import           Prelude                           hiding ((.))
import           Control.Arrow
import           Control.Category                  ((.))
import qualified Control.Categorical.Functor as F
import           Control.PseudoInverseCategory
import           Data.Kind
import           Data.String
import           Data.Text
import           Language.Javascript.JSaddle
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp
#endif
import           UnliftIO.STM

import           Shpadoinkle.Continuation


-- | This is the core type in Backend.
-- The (Html m) 'Functor' is used to describe HTML documents.
-- Please note, this is NOT the Virtual DOM used by Backend.
-- This type backs a DSL that is then /interpreted/ into Virtual DOM
-- by the backend of your choosing. HTML comments are not supported.
data Html :: (Type -> Type) -> Type -> Type where
  -- | A standard node in the dom tree
  Node :: Text -> [(Text, Prop m o)] -> [Html m o] -> Html m o
  -- | If you can bake an element into a 'RawNode' then you can embed it as a baked potato.
  -- Backend does not provide any state management or abstraction to deal with
  -- custom embedded content. It's on you to decide how and when this 'RawNode' will
  -- be updated. For example, if you wanted to embed a google map as a baked potato,
  -- and you are driving your Backend view with a 'TVar', you would need to build
  -- the 'RawNode' for this map /outside/ of your Backend view, and pass it in
  -- as an argument. The 'RawNode' is a reference you control.
  Potato :: JSM RawNode -> Html m o
  -- | The humble text node
  TextNode :: Text -> Html m o


-- | Natural Transformation
type m ~> n = forall a. m a -> n a


-- | If you can provide a Natural Transformation from one Monad to another
-- then you may change the action of @Html@
mapHtml :: Functor m => (m ~> n) -> Html m o -> Html n o
mapHtml f = \case
  Node t ps cs -> Node t (fmap (mapProp f) <$> ps) (mapHtml f <$> cs)
  Potato p -> Potato p
  TextNode t -> TextNode t


-- | If you can provide a Natural Transformation from one Monad to another
-- then you may change the action of @Prop@
mapProp :: Functor m => (m ~> n) -> Prop m o -> Prop n o
mapProp f = \case
  PListener g -> PListener (\x y -> convertC f <$> g x y)
  PText t     -> PText t
  PFlag b     -> PFlag b


instance Monad m => F.Functor EndoIso EndoIso (Html m) where
  map (EndoIso f g i) = EndoIso (mapMC . mapply $ map' (mendo f))
                                (mapMC . mapply $ map' (miso g i))
                                (mapMC . mapply $ map' (miso i g))
    where map' :: EndoIso a b -> EndoIso (Continuation m a) (Continuation m b)
          map' = F.map


instance MapContinuations Html where
  mapMC f (Node t ps es) = Node t (unMapProps . mapMC f $ MapProps ps) (mapMC f <$> es)
  mapMC _ (Potato p) = Potato p
  mapMC _ (TextNode t) = TextNode t


-- | Properties of a DOM node. Backend does not use attributes directly,
-- but rather is focused on the more capable properties that may be set on a dom
-- node in JavaScript. If you wish to add attributes, you may do so
-- by setting its corresponding property.
data Prop m o where
  -- | A text property
  PText :: Text -> Prop m o
  -- | Event listeners are provided with the 'RawNode' target, and the 'RawEvent', and may perform
  -- a monadic action such as a side effect. This is the one and only place where you may
  -- introduce a custom monadic action.
  PListener :: (RawNode -> RawEvent -> JSM (Continuation m o)) -> Prop m o
  -- | A boolean property, works as a flag
  -- for example @("disabled", PFlag False)@ has no effect
  -- while @("disabled", PFlag True)@ will add the @disabled@ attribute
  PFlag :: Bool -> Prop m o


-- | Strings are overloaded as HTML text nodes
-- @
--   "hiya" = TextNode "hiya"
-- @
instance IsString (Html m o) where
  fromString = TextNode . pack
  {-# INLINE fromString #-}


-- | Strings are overloaded as text props
-- @
--   ("id", "foo") = ("id", PText "foo")
-- @
instance IsString (Prop m o) where
  fromString = PText . pack
  {-# INLINE fromString #-}


-- | Strings are overloaded as the class property
-- @
--   "active" = ("className", PText "active")
-- @
instance {-# OVERLAPPING #-} IsString [(Text, Prop m o)] where
  fromString = pure . ("className", ) . PText . pack
  {-# INLINE fromString #-}


instance Monad m => F.Functor EndoIso EndoIso (Prop m) where
  map :: forall a b. EndoIso a b -> EndoIso (Prop m a) (Prop m b)
  map f = EndoIso id mapFwd mapBack
    where f' :: EndoIso (Continuation m a) (Continuation m b)
          f' = F.map f

          mapFwd (PText t) = PText t
          mapFwd (PListener g) = PListener (\r e -> mapply f' <$> g r e)
          mapFwd (PFlag b) = PFlag b

          mapBack (PText t) = PText t
          mapBack (PListener g) = PListener (\r e -> mapply (minverse f') <$> g r e)
          mapBack (PFlag b) = PFlag b


instance MapContinuations Prop where
  mapMC _ (PText t) = PText t
  mapMC f (PListener g) = PListener (\r e -> f <$> g r e)
  mapMC _ (PFlag b) = PFlag b


-- | Type alias for convenience. Typing out the nested brackets is tiresome.
type Props m o = [(Text, Prop m o)]


-- | Newtype to deal with the fact that we can't make the typeclass instances
--   for Endofunctor EndoIso and MapContinuations using the Props type alias.
newtype MapProps m o = MapProps { unMapProps :: Props m o }


instance Monad m => F.Functor EndoIso EndoIso (MapProps m) where
  map f = miso MapProps unMapProps . fmapA (msecond (F.map f)) . miso unMapProps MapProps


instance MapContinuations MapProps where
  mapMC f = MapProps . fmap (second (mapMC f)) . unMapProps


-- | A DOM node reference.
-- Useful for building baked potatoes, and binding a Backend view to the page
newtype RawNode  = RawNode  { unRawNode  :: JSVal }
-- | A raw event object reference
newtype RawEvent = RawEvent { unRawEvent :: JSVal }
instance ToJSVal   RawNode where toJSVal   = return . unRawNode
instance FromJSVal RawNode where fromJSVal = return . Just . RawNode


-- |
-- patch raw Nothing >=> patch raw Nothing = patch raw Nothing

-- | The Backend class describes a backend that can render 'Html'.
-- Backends are generally Monad Transformers @b@ over some Monad @m@.
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
  setup :: JSM () -> b m ()


-- | The core view instantiation function.
-- This combines a backend, a territory, and a model
-- and renders the Backend view to the page.
shpadoinkle
  :: forall b m a
   . Backend b m a => Eq a
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
    go c n a = do
      !m  <- j $ interpret toJSM (view a)
      j $ patch c (Just n) m

  j . setup $ do
    c <- j stage
    n <- j $ interpret toJSM (view initial)
    _ <- shouldUpdate (go c) n model
    _ <- j $ patch c Nothing n :: JSM (VNode b m)
    return ()

-- | Wrapper around @shpadoinkle@ for full page apps
-- that do not need outside control of the territory
fullPage
  :: Backend b m a => Eq a
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


-- | Wrapper around @shpadoinkle@ for full page apps
-- that do not need outside control of the territory
-- where actions are performed directly in JSM.
--
-- This set of assumptions is extremely common when starting
-- a new project.
fullPageJSM
  :: Backend b JSM a => Eq a
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
  :: Backend b JSM a => Eq a
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
