{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}


module Shpadoinkle.Run (
  -- * Agnostic Run
  runJSorWarp
  , runJSorWarpWithIndex
  -- * Live Reloads
  , Env(..), Port
  , liveWithBackend
  , liveWithBackendAndIndex
  , liveWithStatic
  , liveWithStaticAndIndex
  , live
  , liveWithIndex
  -- ** Convenience Variants
  , fullPage
  , fullPageJSM
  , simple
  , entrypoint
  ) where


import           Data.Text                              (Text)
import           Data.ByteString.Lazy                   (ByteString)
import           GHCJS.DOM.Types                        (JSM)
import           Shpadoinkle                            (Backend, Html, RawNode,
                                                         TVar, newTVarIO,
                                                         shpadoinkle, type (~>))


#ifndef ghcjs_HOST_OS


import           Language.Javascript.JSaddle.Warp       (run, runWithIndex)
import           Language.Javascript.JSaddle.WebSockets (debug, debugWithIndex, debugOr, debugWithIndexOr)
import           Network.Wai                            (Application)
import           Network.Wai.Application.Static         (defaultFileServerSettings,
                                                         staticApp)


-- | Serve a web server and a jsaddle warp frontend at the same time.
-- This is useful for live reloads for development purposes.
-- For example:
-- @
--   ghcid -c "cabal repl dev" -W -T "Main.main"
-- @
liveWithBackend
  :: Port
  -- ^ Port to serve the live server
  -> JSM ()
  -- ^ Frontend application
  -> IO Application
  -- ^ Server API
  -> IO ()
liveWithBackend port frontend server = debugOr port frontend =<< server


-- | Identical to 'liveWithBackend', but with a custom @index.html@ file.
liveWithBackendAndIndex
  :: ByteString
  -- ^ Custom @index.html@
  -> Port
  -- ^ Port to serve the live server
  -> JSM ()
  -- ^ Frontend application
  -> IO Application
  -- ^ Server API
  -> IO ()
liveWithBackendAndIndex idx port frontend server = debugWithIndexOr idx port frontend =<< server


-- | Serve jsaddle warp frontend.
-- This is useful for live reloads for development purposes.
-- For example:
-- @
--   ghcid -c "cabal repl" -W -T "Main.dev"
-- @
live
  :: Port
  -- ^ Port to serve the live server
  -> JSM ()
  -- ^ Frontend application
  -> IO ()
live = debug


-- | Identical to 'live', but with a custom @index.html@ file.
liveWithIndex
  :: ByteString
  -- ^ Custom @index.html@
  -> Port
  -- ^ Port to serve the live server
  -> JSM ()
  -- ^ Frontend application
  -> IO ()
liveWithIndex = debugWithIndex


-- | Serve jsaddle warp frontend with a static file server.
liveWithStatic
  :: Port
  -- ^ Port to serve the live server
  -> JSM ()
  -- ^ Frontend application
  -> FilePath
  -- ^ Path to static files
  -> IO ()
liveWithStatic port frontend =
  liveWithBackend port frontend . pure . staticApp . defaultFileServerSettings


-- | Identical to 'liveWithStatic', but with a custom @index.html@ file.
liveWithStaticAndIndex
  :: ByteString
  -- ^ Custom @index.html@
  -> Port
  -- ^ Port to serve the live server
  -> JSM ()
  -- ^ Frontend application
  -> FilePath
  -- ^ Path to static files
  -> IO ()
liveWithStaticAndIndex idx port frontend =
  liveWithBackendAndIndex idx port frontend . pure . staticApp . defaultFileServerSettings


#else


data Application


live :: Port -> JSM () -> IO ()
live = error "Live reloads require GHC"


liveWithIndex :: ByteString -> Port -> JSM () -> IO ()
liveWithIndex = error "Live reloads require GHC"


liveWithStatic :: Port -> JSM () -> FilePath -> IO ()
liveWithStatic = error "Live reloads require GHC"


liveWithStaticAndIndex :: ByteString -> Port -> JSM () -> FilePath -> IO ()
liveWithStaticAndIndex = error "Live reloads require GHC"


liveWithBackend :: Port -> JSM () -> IO Application -> IO ()
liveWithBackend = error "Live reloads require GHC"


liveWithBackendAndIndex :: ByteString -> Port -> JSM () -> IO Application -> IO ()
liveWithBackendAndIndex = error "Live reloads require GHC"


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


-- | Start the program (with a custom @index.html@)!
--
-- This function works in GHC and GHCjs. I saved you from using C preprocessor directly. You're welcome.
runJSorWarpWithIndex :: ByteString -> Int -> JSM () -> IO ()
#ifdef ghcjs_HOST_OS
runJSorWarpWithIndex _ _ = id
{-# INLINE runJSorWarpWithIndex #-}
#else
runJSorWarpWithIndex = runWithIndex
{-# INLINE runJSorWarpWithIndex #-}
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


entrypoint :: Env -> Text
entrypoint Dev  = "/jsaddle.js"
entrypoint Prod = "/all.min.js"
