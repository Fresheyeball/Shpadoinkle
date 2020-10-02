{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


-- | Since the single page application URIs are specified with Servant, we can automate much
-- of the process of serving the application with server-side rendering. This module provides
-- the basic infrastructure for serving rendered HTML using the same code that would be used
-- to render the same route on the client-side, ensuring a consistent rendering, whether a
-- URI is accessed via a client-side popstate event or via the initial page load.


module Shpadoinkle.Router.Server where


#ifndef ghcjs_HOST_OS


import           Data.ByteString.Lazy           as BS
import           Data.Text.Encoding
import           GHC.TypeLits
import           Network.Wai
import           Network.Wai.Application.Static
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles
import           WaiAppStatic.Types

import           Shpadoinkle
import           Shpadoinkle.Backend.Static
import           Shpadoinkle.Router


-- | Helper to serve a 'ByteString' as a file from the web application interface.
toFile :: Piece -> ByteString -> File
toFile p bs = File
  { fileGetSize     = fromIntegral $ BS.length bs
  , fileToResponse  = \status headers -> responseLBS status headers bs
  , fileName        = p
  , fileGetHash     = pure Nothing
  , fileGetModified = Nothing
  }


-- | Serve index.html generated from a Shpadoinkle view using the static backend, otherwise serve out of a directory.
defaultSPAServerSettings
  :: FilePath
  -- ^ Directory to try files
  -> IO (Html m a)
  -- ^ Get the index.html page
  -> StaticSettings
defaultSPAServerSettings root mhtml = settings { ssLookupFile = orIndex, ssMaxAge = MaxAgeSeconds 0 }
  where

  settings   = defaultWebAppSettings root

  orIndex ps = do
    let file ps' = toFile ps' . BS.fromStrict . encodeUtf8 . renderStatic
    res <- ssLookupFile settings ps
    html <- mhtml
    return $ case (res, toPieces ["index.html"]) of
      (LRNotFound, Just [ps'])                                  -> LRFile $ file ps' html
      (_,          Just [ps']) | [ps'] == ps || Prelude.null ps -> LRFile $ file ps' html
      _                                                         -> res


-- | Serve the UI by generating a Servant Server from the SPA URIs
class ServeRouter layout route where
  serveUI
    :: FilePath
    -- ^ Where should we look for static assets?
    -> (route -> IO (Html m a))
    -- ^ How shall we get the page based on the requested route?
    -> layout :>> route
    -- ^ What is the relationship between URIs and routes?
    -> Server layout


instance (ServeRouter x r, ServeRouter y r)
  => ServeRouter (x :<|> y) r where

  serveUI :: FilePath -> (r -> IO (Html m a)) -> (x :<|> y) :>> r -> Server (x :<|> y)
  serveUI root view (x :<|> y) = serveUI @x root view x :<|> serveUI @y root view y
  {-# INLINABLE serveUI #-}

instance ServeRouter sub r
  => ServeRouter (Capture sym x :> sub) r where

  serveUI :: FilePath -> (r -> IO (Html m a)) -> (x -> sub :>> r) -> Server (Capture sym x :> sub)
  serveUI root view = (serveUI @sub root view .)
  {-# INLINABLE serveUI #-}

instance ServeRouter sub r
  => ServeRouter (QueryParam sym x :> sub) r where

  serveUI :: FilePath -> (r -> IO (Html m a)) -> (Maybe x -> sub :>> r) -> Server (QueryParam sym x :> sub)
  serveUI root view = (serveUI @sub root view .)
  {-# INLINABLE serveUI #-}

instance ServeRouter sub r
  => ServeRouter (QueryParams sym x :> sub) r where

  serveUI :: FilePath -> (r -> IO (Html m a)) -> ([x] -> sub :>> r) -> Server (QueryParams sym x :> sub)
  serveUI root view = (serveUI @sub root view .)
  {-# INLINABLE serveUI #-}

instance ServeRouter sub r
  => ServeRouter (QueryFlag sym :> sub) r where

  serveUI :: FilePath -> (r -> IO (Html m a)) -> (Bool -> sub :>> r) -> Server (QueryFlag sym :> sub)
  serveUI root view = (serveUI @sub root view .)
  {-# INLINABLE serveUI #-}

instance ServeRouter sub r
  => ServeRouter ((path :: Symbol) :> sub) r where

  serveUI :: FilePath -> (r -> IO (Html m a)) -> (path :> sub) :>> r -> Server (path :> sub)
  serveUI = serveUI @sub
  {-# INLINABLE serveUI #-}

instance ServeRouter Raw r where
  serveUI :: FilePath -> (r -> IO (Html m a)) -> Raw :>> r -> Server Raw
  serveUI root view = serveDirectoryWith . defaultSPAServerSettings root . view
  {-# INLINABLE serveUI #-}

#endif
