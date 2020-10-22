{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}


module Shpadoinkle.Run (
  -- * Agnostic Run
  runJSorWarp,
  -- * Live Reloads
  Env(..), Port
#ifndef ghcjs_HOST_OS
  , liveWithBackend
  , live
#endif
  -- ** Convenience Variants
  , fullPage
  , fullPageJSM
  , simple
  ) where


import           GHCJS.DOM.Types                        (JSM)
import           Shpadoinkle                            (Backend, Html, RawNode,
                                                         TVar, type (~>),
                                                         newTVarIO, shpadoinkle)


#ifndef ghcjs_HOST_OS


import           Language.Javascript.JSaddle.Warp       (run)
import           Language.Javascript.JSaddle.WebSockets (debug, debugOr)
import           Network.Wai                            (Application)


-- | Serve a web server and a jsaddle warp frontend at the same time.
-- This is useful for live reloads for development purposes.
-- For example:
-- @
--   ghcid -c "cabal repl dev" -W -T "Main.main"
-- @
liveWithBackend
  :: Port
  -- ^ Port to server the live server
  -> JSM ()
  -- ^ Frontend application
  -> IO Application
  -- ^ Server API
  -> IO ()
liveWithBackend port frontend server = debugOr port frontend =<< server


-- | Serve jsaddle warp frontend.
-- This is useful for live reloads for development purposes.
-- For example:
-- @
--   ghcid -c "cabal repl" -W -T "Main.dev"
-- @
live
  :: Port
  -- ^ Port to server the live server
  -> JSM ()
  -- ^ Frontend application
  -> IO ()
live = debug


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
  shpadoinkle g f i model view getStage
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


-- | Start the program!
--
-- This function works in GHC and GHCjs. I saved you from using C preprocessor directly. You're welcome.
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
