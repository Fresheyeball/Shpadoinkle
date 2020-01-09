{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Shpadoinkle.Router
    ( View, Redirect(..), routeWith
    , fromRouter, HasRouter(..), HasLink(..), patchIncompleteURI) where

import           Control.Applicative
import           Data.Maybe          (catMaybes, isJust)
import           Data.Monoid         ((<>))
import           Data.Proxy
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import           GHC.TypeLits
import qualified Network.URI         as URI
import           Servant.API         hiding (uriPath, uriQuery)
import           Servant.Utils.Links (Link, linkURI, safeLink)
import           Web.HttpApiData     (parseQueryParamMaybe, parseUrlPieceMaybe)


-- | A stub to represent HTML rendering on the page
data View


-- | Term level API representation
data Router a where
    RChoice :: Router a -> Router a -> Router a
    RCapture :: FromHttpApiData x => (x -> Router a) -> Router a
    RQueryParam :: (FromHttpApiData x, KnownSymbol sym) => Proxy sym -> (Maybe x -> Router a) -> Router a
    RQueryParams :: (FromHttpApiData x, KnownSymbol sym) => Proxy sym -> ([x] -> Router a) -> Router a
    RQueryFlag :: KnownSymbol sym => Proxy sym -> (Bool -> Router a) -> Router a
    RPath :: KnownSymbol sym => Proxy sym -> Router a -> Router a
    RView :: a -> Router a


-- | Redirect is an existentialized Proxy that must be a member of the API
data Redirect api
    = forall sub. (IsElem sub api, HasLink sub)
    => Redirect !(Proxy sub) !(MkLink sub -> Link)


-- | Main routing function
routeWith
    :: forall t m layout r. HasRouter layout
    => Proxy layout
    -> layout `RoutedAs` r
    -> Redirect layout
    -> m ()
routeWith _ handlers er = undefined


-- | Parse a url part to some Servant data
parseSeg :: FromHttpApiData a => Text -> Maybe a
parseSeg = parseQueryParamMaybe


-- | Get an @r@ from a route and url context
fromRouter :: Text -> [Text] -> Router r -> Maybe r
fromRouter queries segs = \case
    RChoice x y        -> fromRouter queries segs x <|> fromRouter queries segs y
    RCapture f         -> case segs of
        []                 -> Nothing
        capture:paths      -> fromRouter queries paths . f =<< parseUrlPieceMaybe capture
    RQueryParam sym f  ->
        case lookup (BS.pack $ symbolVal sym) (BU.queryPairs queries) of
            Nothing -> fromRouter queries segs $ f Nothing
            Just t  -> fromRouter queries segs $ f (parseQueryParamMaybe t)
    RQueryParams sym f ->
        fromRouter queries segs . f . catMaybes $ parseQueryParamMaybe . snd <$> filter
            (\(k, _) -> k == BS.pack (symbolVal sym))
            (BU.queryPairs queries)
    RQueryFlag sym f ->
        fromRouter queries segs . f . isJust $ lookup (BS.pack $ symbolVal sym) (BU.queryPairs queries)
    RPath sym a        -> case segs of
        []                 -> Nothing
        p:paths            -> if p == T.pack (symbolVal sym) then
            fromRouter queries paths a else Nothing
    RView a            -> if null segs then Just a else Nothing


------------------------------------------------------------------------------
-- | This type class traverses the Servant API, and sets up a function to
-- build its term level representation.
class HasRouter layout where
    -- | RoutedAs should be surjective.
    -- As in one route can be the handler for more than one url.
    type RoutedAs layout route :: *
    route :: Proxy layout -> Proxy route -> RoutedAs layout route -> Router route

instance (HasRouter x, HasRouter y) => HasRouter (x :<|> y) where
    type RoutedAs (x :<|> y) r = RoutedAs x r :<|> RoutedAs y r
    route _ (r :: Proxy r) (x :<|> y) =
        RChoice (route (Proxy @x) r x) (route (Proxy @y) r y)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x)
    => HasRouter (Capture sym x :> sub) where
    type RoutedAs (Capture sym x :> sub) a = x -> RoutedAs sub a
    route _ (r :: Proxy r) = RCapture . (route (Proxy @sub) r .)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParam sym x :> sub) where
    type RoutedAs (QueryParam sym x :> sub) a = Maybe x -> RoutedAs sub a
    route _ (r :: Proxy r) = RQueryParam (Proxy @sym) . (route (Proxy @sub) r .)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParams sym x :> sub) where
    type RoutedAs (QueryParams sym x :> sub) a = [x] -> RoutedAs sub a
    route _ (r :: Proxy r) = RQueryParams (Proxy @sym) . (route (Proxy @sub) r .)
    {-# INLINABLE route #-}

instance (HasRouter sub, KnownSymbol sym)
    => HasRouter (QueryFlag sym :> sub) where
    type RoutedAs (QueryFlag sym :> sub) a = Bool -> RoutedAs sub a
    route _ (r :: Proxy r) = RQueryFlag (Proxy @sym) . (route (Proxy @sub) r .)

instance (HasRouter sub, KnownSymbol path)
    => HasRouter (path :> sub) where
    type RoutedAs (path :> sub) a = RoutedAs sub a
    route _ (r :: Proxy r) = RPath (Proxy @path) . route (Proxy @sub) r
    {-# INLINABLE route #-}

instance HasRouter View where
    type RoutedAs View a = a
    route _ _ = RView
    {-# INLINABLE route #-}

instance HasLink View where
  type MkLink View = Link
  toLink _ = id
  {-# INLINABLE toLink #-}
