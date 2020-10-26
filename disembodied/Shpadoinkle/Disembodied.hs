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
import           Data.Text                  (unpack)
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
data Site ctx where

  -- | A path segment in the URI
  SPath
    :: String
    -- ^ The current URI path segment
    -> Site ctx
    -- ^ The site to be rendered at the path
    -> Site ctx

  -- | Html to be rendered as @index.html@
  SIndex
    :: forall m a ctx
     . (ctx -> Html m a)
     -- ^ Given a context, how can we render a page?
     -> Site ctx

  -- | Capture is the one Servant combinator that can be meaningful in static
  -- site generation, and only if we can generate all possible instances.
  SCapture
    :: (FromHttpApiData x, ToHttpApiData x, Bounded x, Enum x)
    => (x -> Site ctx)
    -- ^ Given a context, provide the remaining site to be generated
    -> Site ctx

  -- | Branch the site at a given point in generation.
  SChoice :: Site ctx -> Site ctx -> Site ctx



-- | Type class induction for building the site out of a specification
class Disembodied ctx a where
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
  type SiteSpec ctx a :: Type

  -- | Construct the site structure out of the associated API
  buildSite :: SiteSpec ctx a -> Site ctx

instance (Disembodied ctx x, Disembodied ctx y)
  => Disembodied ctx (x :<|> y) where

  type SiteSpec ctx (x :<|> y) = SiteSpec ctx x :<|> SiteSpec ctx y

  buildSite :: SiteSpec ctx x :<|> SiteSpec ctx y -> Site ctx
  buildSite (x :<|> y) = SChoice (buildSite @ctx @x x) (buildSite @ctx @y y)
  {-# INLINABLE buildSite #-}

instance (Disembodied ctx sub, KnownSymbol path)
  => Disembodied ctx (path :> sub) where

  type SiteSpec ctx (path :> sub) = SiteSpec ctx sub

  buildSite :: SiteSpec ctx sub -> Site ctx
  buildSite = SPath (symbolVal (Proxy @path)) . buildSite @ctx @sub
  {-# INLINABLE buildSite #-}

instance (Disembodied ctx sub, FromHttpApiData x, ToHttpApiData x, Bounded x, Enum x)
  => Disembodied ctx (Capture sym x :> sub) where

  type SiteSpec ctx (Capture sym x :> sub) = x -> SiteSpec ctx sub

  buildSite :: (x -> SiteSpec ctx sub) -> Site ctx
  buildSite = SCapture . (buildSite @ctx @sub .)
  {-# INLINABLE buildSite #-}

instance Disembodied ctx sub
  => Disembodied ctx (QueryParam sym x :> sub) where

  type SiteSpec ctx (QueryParam sym x :> sub) = Maybe x -> SiteSpec ctx sub

  buildSite :: (Maybe x -> SiteSpec ctx sub) -> Site ctx
  buildSite f = buildSite @ctx @sub $ f Nothing
  {-# INLINABLE buildSite #-}

instance Disembodied ctx sub
  => Disembodied ctx (QueryParam' ms sym x :> sub) where

  type SiteSpec ctx (QueryParam' ms sym x :> sub) = Maybe x -> SiteSpec ctx sub

  buildSite :: (Maybe x -> SiteSpec ctx sub) -> Site ctx
  buildSite f = buildSite @ctx @sub $ f Nothing
  {-# INLINABLE buildSite #-}

instance Disembodied ctx sub
  => Disembodied ctx (QueryParams sym x :> sub) where

  type SiteSpec ctx (QueryParams sym x :> sub) = [x] -> SiteSpec ctx sub

  buildSite :: ([x] -> SiteSpec ctx sub) -> Site ctx
  buildSite f = buildSite @ctx @sub $ f []
  {-# INLINABLE buildSite #-}

instance Disembodied ctx sub
  => Disembodied ctx (QueryFlag sym :> sub) where

  type SiteSpec ctx (QueryFlag sym :> sub) = Bool -> SiteSpec ctx sub

  buildSite :: (Bool -> SiteSpec ctx sub) -> Site ctx
  buildSite f = buildSite @ctx @sub $ f False
  {-# INLINABLE buildSite #-}

instance Disembodied ctx (f '[HTML] (Html m a)) where

  type SiteSpec ctx (f '[HTML] (Html m a)) = ctx -> Html m a

  buildSite :: (ctx -> Html m a) -> Site ctx
  buildSite = SIndex
  {-# INLINABLE buildSite #-}

instance Disembodied ctx (View m a) where

  type SiteSpec ctx (View m a) = ctx -> Html m a

  buildSite :: (ctx -> Html m a) -> Site ctx
  buildSite = SIndex
  {-# INLINABLE buildSite #-}


-- | Actually write the site to disk. Branches are written in parallel.
writeSite
  :: forall layout ctx. Disembodied ctx layout
  => FilePath
  -- ^ Out path
  -> ctx
  -- ^ Universal context for the static site.
  -> SiteSpec ctx layout
  -- ^ Specification for the pages of the site relative to a Servant API.
  -> IO ()
writeSite fs ctx layout = go fs $ buildSite @ctx @layout layout where

  go :: FilePath -> Site ctx -> IO ()
  go curr (SIndex page) = T.writeFile (curr </> "index" <.> "html") . renderStatic $ page ctx
  go curr (SChoice x y) = void $ go curr x `concurrently` go curr y
  go curr (SCapture f)  = forConcurrently_ [ minBound .. maxBound ] $
    \c -> go curr $ SPath (unpack $ toUrlPiece c) $ f c
  go curr (SPath path site) = do
    createDirectoryIfMissing False (curr </> path)
    go (curr </> path) site
