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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


-- | Since the single page application URIs are specified with Servant, we can automate much
-- of the process of serving the application with server-side rendering. This module provides
-- the basic infrastructure for serving rendered HTML using the same code that would be used
-- to render the same route on the client-side, ensuring a consistent rendering, whether a
-- URI is accessed via a client-side popstate event or via the initial page load.


module Shpadoinkle.Router.Server where


#ifndef ghcjs_HOST_OS


import           Data.ByteString.Lazy           as BS (ByteString, length)
import           Data.Text.Lazy.Encoding        (encodeUtf8)
import           Network.HTTP.Types             (hContentType, status200)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Application.Static (StaticSettings (ssLookupFile, ssMaxAge),
                                                 defaultWebAppSettings,
                                                 staticApp)
import           Servant.API                    (Capture, QueryFlag, QueryParam,
                                                 QueryParams, Raw,
                                                 type (:<|>) (..), type (:>))
import           Servant.Server                 (HasServer (ServerT), Server)
import           Servant.Server.StaticFiles     (serveDirectoryWith)
import           WaiAppStatic.Types             (File (..),
                                                 LookupResult (LRFile, LRNotFound),
                                                 MaxAge (MaxAgeSeconds), Piece,
                                                 toPieces)

import           Data.Kind                      (Constraint)
import           GHC.TypeLits
import           Shpadoinkle                    (Html)
import           Shpadoinkle.Backend.Static     (renderStatic)
import           Shpadoinkle.Router             (HasRouter (..), View)


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
    let file ps' = toFile ps' . encodeUtf8 . renderStatic
    res <- ssLookupFile settings ps
    html <- mhtml
    case (res, toPieces ["index.html"]) of
      (LRNotFound, Just [ps'])
        -> LRFile (file ps' html) <$ putStrLn ("LRNotFound " <> show ps)
      (_,          Just [ps'])
        | [ps'] == ps || Prelude.null ps
        -> LRFile (file ps' html) <$ putStrLn ("PRED " <> show ps)
      _ -> res <$ putStrLn ("RES " <> show ps)


-- | This is needed in order for file serving to work.
-- If there is no Raw root route, there is no route to serve files
-- such as ./all.min.js which is required for obvious reasons
type family HasRoot l :: Constraint where
  HasRoot (_ :<|> Raw) = ()
  HasRoot (_ :<|> l') = HasRoot l'
  HasRoot _ = TypeError
    ('Text "Your SPA type lacks \n          :<|> Raw \n      at the end. This is important, because without this we have no way to \n      serve the static assets. Please add :<|> Raw as the final alternative \n      in your SPA type.")


serveUI
  :: forall layout route m a
   . ServeRouter layout route m a
  => HasRoot layout
  => FilePath
  -- ^ Where should we look for static assets?
  -> (route -> IO (Html m a))
  -- ^ How shall we get the page based on the requested route?
  -> layout :>> route
  -- ^ What is the relationship between URIs and routes?
  -> Server layout
serveUI = serveUIUnsafe @layout


-- | Serve the UI by generating a Servant Server from the SPA URIs
class ServeRouter layout route m a where
  serveUIUnsafe
    :: FilePath
    -- ^ Where should we look for static assets?
    -> (route -> IO (Html m a))
    -- ^ How shall we get the page based on the requested route?
    -> layout :>> route
    -- ^ What is the relationship between URIs and routes?
    -> Server layout


instance (ServeRouter x r m a, ServeRouter y r m a)
  => ServeRouter (x :<|> y) r m a where

  serveUIUnsafe :: FilePath -> (r -> IO (Html m a)) -> (x :<|> y) :>> r -> Server (x :<|> y)
  serveUIUnsafe root view (x :<|> y) = serveUIUnsafe @x root view x :<|> serveUIUnsafe @y root view y
  {-# INLINABLE serveUIUnsafe #-}

instance ServeRouter sub r m a
  => ServeRouter (Capture sym x :> sub) r m a where

  serveUIUnsafe :: FilePath -> (r -> IO (Html m a)) -> (x -> sub :>> r) -> Server (Capture sym x :> sub)
  serveUIUnsafe root view = (serveUIUnsafe @sub root view .)
  {-# INLINABLE serveUIUnsafe #-}

instance ServeRouter sub r m a
  => ServeRouter (QueryParam sym x :> sub) r m a where

  serveUIUnsafe :: FilePath -> (r -> IO (Html m a)) -> (Maybe x -> sub :>> r) -> Server (QueryParam sym x :> sub)
  serveUIUnsafe root view = (serveUIUnsafe @sub root view .)
  {-# INLINABLE serveUIUnsafe #-}

instance ServeRouter sub r m a
  => ServeRouter (QueryParams sym x :> sub) r m a where

  serveUIUnsafe :: FilePath -> (r -> IO (Html m a)) -> ([x] -> sub :>> r) -> Server (QueryParams sym x :> sub)
  serveUIUnsafe root view = (serveUIUnsafe @sub root view .)
  {-# INLINABLE serveUIUnsafe #-}

instance ServeRouter sub r m a
  => ServeRouter (QueryFlag sym :> sub) r m a where

  serveUIUnsafe :: FilePath -> (r -> IO (Html m a)) -> (Bool -> sub :>> r) -> Server (QueryFlag sym :> sub)
  serveUIUnsafe root view = (serveUIUnsafe @sub root view .)
  {-# INLINABLE serveUIUnsafe #-}

instance ServeRouter sub r m a
  => ServeRouter ((path :: Symbol) :> sub) r m a where

  serveUIUnsafe :: FilePath -> (r -> IO (Html m a)) -> (path :> sub) :>> r -> Server (path :> sub)
  serveUIUnsafe = serveUIUnsafe @sub
  {-# INLINABLE serveUIUnsafe #-}

instance ServeRouter Raw r m a where
  serveUIUnsafe :: FilePath -> (r -> IO (Html m a)) -> Raw :>> r -> Server Raw
  serveUIUnsafe root view = serveDirectoryWith . defaultSPAServerSettings root . view
  {-# INLINABLE serveUIUnsafe #-}


instance ServeRouter (View n b) r m a where
  serveUIUnsafe :: FilePath -> (r -> IO (Html m a)) -> View n b :>> r -> Server (View n b)
  serveUIUnsafe root view = serveDirectoryWithSpa . defaultSPAServerSettings root . view
  {-# INLINABLE serveUIUnsafe #-}


serveDirectoryWithSpa :: Applicative n => StaticSettings -> ServerT (View m a) n
serveDirectoryWithSpa = pure . staticApp


serveHtml :: Html m a -> Application
serveHtml html _ respond
  = respond . responseLBS status200 [(hContentType, "text/html")]
  . encodeUtf8 $ renderStatic html
#endif
