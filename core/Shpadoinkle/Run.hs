{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}


module Shpadoinkle.Run
  ( Env(..), Port
  -- ** Convenience Variants
  , fullPage
  , fullPageJSM
  , simple
  , run
  , entrypoint
  ) where


import           Data.Text         (Text)
import           Shpadoinkle       (Backend, Html, RawNode, TVar, newTVarIO,
                                    shpadoinkle, type (~>))
import           Shpadoinkle.JSFFI (JSM)
#ifndef ghcjs_HOST_OS
import           Shpadoinkle.JSFFI (ghcjsOnly)
#endif



data Env = Dev | Prod


type Port = Int


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
  shpadoinkle g f model view getStage
{-# INLINE fullPage #-}


-- | 'fullPageJSM' is a wrapper around 'shpadoinkle'
-- for full page apps that do not need outside control
-- of the territory, where actions are performed directly in JSM.
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


-- | Simple app
--
-- (a good starting place)
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


-- | Start the program!
run :: JSM () -> IO ()
#ifdef ghcjs_HOST_OS
run = id
#else
run = ghcjsOnly
#endif
{-# INLINE run #-}


entrypoint :: Env -> Text
entrypoint Dev  = "/jsaddle.js"
entrypoint Prod = "/all.min.js"
