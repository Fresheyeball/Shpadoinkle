{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


{-|
   Static sites are the opposite of headless applications, they are disembodied.
   Shpadoinkle Disembodied is a static site generator for Shpadoinkle applications.
-}


module Shpadoinkle.Disembodied (
  -- * Site Reification
  Site(..)
  -- * Class
  , Disembodied(..)
  -- * Write Files
  , writeSite
  ) where


import           Control.Monad              (void)
import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (isSuffixOf, pack, unpack)
import           Data.Text.IO               as T (writeFile)
import           Servant.API
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((<.>), (</>))
import           UnliftIO.Async             (concurrently, forConcurrently_)

import           GHC.TypeLits               (KnownSymbol, symbolVal)
import           Shpadoinkle                (Html)
import           Shpadoinkle.Backend.Static (renderStatic)
import           Shpadoinkle.Router         (HTML, View)


-- | The reification of a static site based on Servant routes.
-- Site takes a context `ctx` which is universal for the static site.
-- This is useful for storing commonly used valus like the site name,
-- site url, copyright date, ect.
data Site where

  -- | A path segment in the URI
  SPath
    :: String
    -- ^ The current URI path segment
    -> Site
    -- ^ The site to be rendered at the path
    -> Site

  -- | Html to be rendered as @index.html@
  SIndex
    :: forall m a
     . Html m a
     -- ^ Given a context, how can we render a page?
     -> Site

  -- | Capture is the one Servant combinator that can be meaningful in static
  -- site generation, and only if we can generate all possible instances.
  SCapture
    :: (FromHttpApiData x, ToHttpApiData x, Bounded x, Enum x)
    => (x -> Site)
    -- ^ Given a context, provide the remaining site to be generated
    -> Site

  -- | Branch the site at a given point in generation.
  SChoice :: Site -> Site -> Site



-- | Type class induction for building the site out of a specification
class Disembodied a where
  -- | A type family to represent the relationship between a Servant API
  -- and the 'Html' views to render.
  --
  -- @
  -- type SPA m = "about" :> Html m ()
  --         :\<|\> Html m ()
  --
  -- site :: SiteSpec () (SPA m)
  -- site = const (h1_ [ text "about" ])
  --   :\<|\> const (h1_ [ text "home" ])
  -- @
  type SiteSpec a :: Type

  -- | Construct the site structure out of the associated API
  buildSite :: SiteSpec a -> Site

instance (Disembodied x, Disembodied y)
  => Disembodied (x :<|> y) where

  type SiteSpec (x :<|> y) = SiteSpec x :<|> SiteSpec y

  buildSite :: SiteSpec x :<|> SiteSpec y -> Site
  buildSite (x :<|> y) = SChoice (buildSite @x x) (buildSite @y y)
  {-# INLINABLE buildSite #-}

instance (Disembodied sub, KnownSymbol path)
  => Disembodied (path :> sub) where

  type SiteSpec (path :> sub) = SiteSpec sub

  buildSite :: SiteSpec sub -> Site
  buildSite = SPath (symbolVal (Proxy @path)) . buildSite @sub
  {-# INLINABLE buildSite #-}

instance (Disembodied sub, FromHttpApiData x, ToHttpApiData x, Bounded x, Enum x)
  => Disembodied (Capture sym x :> sub) where

  type SiteSpec (Capture sym x :> sub) = x -> SiteSpec sub

  buildSite :: (x -> SiteSpec sub) -> Site
  buildSite = SCapture . (buildSite @sub .)
  {-# INLINABLE buildSite #-}

instance Disembodied sub
  => Disembodied (QueryParam sym x :> sub) where

  type SiteSpec (QueryParam sym x :> sub) = Maybe x -> SiteSpec sub

  buildSite :: (Maybe x -> SiteSpec sub) -> Site
  buildSite f = buildSite @sub $ f Nothing
  {-# INLINABLE buildSite #-}

instance Disembodied sub
  => Disembodied (QueryParam' ms sym x :> sub) where

  type SiteSpec (QueryParam' ms sym x :> sub) = Maybe x -> SiteSpec sub

  buildSite :: (Maybe x -> SiteSpec sub) -> Site
  buildSite f = buildSite @sub $ f Nothing
  {-# INLINABLE buildSite #-}

instance Disembodied sub
  => Disembodied (QueryParams sym x :> sub) where

  type SiteSpec (QueryParams sym x :> sub) = [x] -> SiteSpec sub

  buildSite :: ([x] -> SiteSpec sub) -> Site
  buildSite f = buildSite @sub $ f []
  {-# INLINABLE buildSite #-}

instance Disembodied sub
  => Disembodied (QueryFlag sym :> sub) where

  type SiteSpec (QueryFlag sym :> sub) = Bool -> SiteSpec sub

  buildSite :: (Bool -> SiteSpec sub) -> Site
  buildSite f = buildSite @sub $ f False
  {-# INLINABLE buildSite #-}

instance Disembodied (f '[HTML] (Html m a)) where

  type SiteSpec (f '[HTML] (Html m a)) = Html m a

  buildSite :: Html m a -> Site
  buildSite = SIndex
  {-# INLINABLE buildSite #-}

instance Disembodied (View m a) where

  type SiteSpec (View m a) = Html m a

  buildSite :: Html m a -> Site
  buildSite = SIndex
  {-# INLINABLE buildSite #-}


-- | Actually write the site to disk. Branches are written in parallel.
writeSite
  :: forall layout. Disembodied layout
  => FilePath
  -- ^ Out path
  -> SiteSpec layout
  -- ^ Specification for the pages of the site relative to a Servant API.
  -> IO ()
writeSite fs layout = go fs $ buildSite @layout layout where

  go :: FilePath -> Site -> IO ()
  go curr (SIndex page) = T.writeFile (curr </> "index" <.> "html") $ renderStatic page
  go curr (SChoice x y) = void $ go curr x `concurrently` go curr y
  go curr (SCapture f)  = forConcurrently_ [ minBound .. maxBound ] $
    \c -> go curr $ SPath (unpack $ toUrlPiece c) $ f c
  go curr (SPath path (SIndex page)) | ".html" `isSuffixOf` pack path = do
    createDirectoryIfMissing False curr
    T.writeFile (curr </> path) $ renderStatic page
  go curr (SPath path site) = do
    createDirectoryIfMissing False (curr </> path)
    go (curr </> path) site
