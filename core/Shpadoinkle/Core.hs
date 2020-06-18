{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}


{-|
   I think I know precisely what I mean.

   A frontend abstraction motivated by simplicity, performance, and ergonomics.
   This module provides core abstractions and types with almost no implementation details. I.e., no batteries included.
   You may use this model a la carte, build on top of it, or include more Backend packages for additional batteries.

   Backend is focused on letting you build your frontend the way you want to. It's as unopinionated as possible, beyond providing a concrete programming model.
-}


module Shpadoinkle.Core
  ( Backend (..)
  , Constantly (..)
  , constly'
  , JSM, TVar, newTVarIO, readTVarIO
  , shpadoinkle
  , runJSorWarp
  , fullPage
  , fullPageJSM
  , simple
  , static
  ) where


import           Control.Arrow                    (second)
import           Control.ShpadoinkleContinuation  (pur, shouldUpdate)
import           Language.Javascript.JSaddle      (JSM)
import           UnliftIO.STM                     (TVar, newTVarIO, readTVarIO)

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp (run)
#endif

import           Shpadoinkle.Class
import           Shpadoinkle.EndoIso
import           Shpadoinkle.Functor


-- | The Backend class describes a backend that can render 'HtmlM'.
-- Backends are generally Monad Transformers @b@ over some Monad @m@.
--
-- prop> patch raw Nothing >=> patch raw Nothing = patch raw Nothing
class Backend b m a | b m -> a where
  -- | VNode type family allows backends to have their own Virtual DOM.
  -- As such we can change out the rendering of our Backend view
  -- with new backends without updating our view logic.
  type VNode b m
  -- | A backend must be able to interpret 'HtmlM' into its own internal Virtual DOM
  interpret
    :: (m ~> JSM)
    -- ^ Natural transformation for some @m@ to 'JSM'.
    -- This is how a Backend gets access to 'JSM' to perform the rendering side effects.
    -> HtmlM (b m) a
    -- ^ 'HtmlM' to interpret
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
  -> (a -> HtmlM (b m) a)
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
  -> (a -> HtmlM (b m) a)
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
  -> (a -> HtmlM (b JSM) a)
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
  -> (a -> HtmlM (b JSM) a)
  -- ^ how should the html look?
  -> b JSM RawNode
  -- ^ where do we render?
  -> JSM ()
simple = fullPageJSM
{-# INLINE simple #-}


class Constantly f g where
  constly :: (a -> b -> b) -> f a -> g b


instance Applicative m => Constantly Html (HtmlM m) where
  constly f (Node t ps es) = NodeM t (second (constly f) <$> ps) (fmap (constly f) es)
  constly _ (Potato p) = PotatoM p
  constly _ (TextNode t) = TextNodeM t


instance Applicative m => Constantly Prop (PropM m) where
  constly _ (PText t)     = PTextM t
  constly f (PListener g) = PListenerM (\r e -> pur . f <$> g r e)
  constly _ (PFlag b)     = PFlagM b


constly' :: Constantly f g => f a -> g a
constly' = constly const


static :: Constantly f g => f a -> g b
static = constly (const id)

