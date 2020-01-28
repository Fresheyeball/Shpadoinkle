{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Router
    ( Raw, Redirect(..)
    , fromRouter
    , HasRouter(..)
    , HasLink(..)
    , Routed(..)
    , fullPageSPA
    , navigate
    ) where


import           Control.Applicative
import           Control.Compactable           as C
import           Data.Kind
import           Data.Maybe                    (isJust)
import           Data.Proxy
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Conc
import           GHC.TypeLits
import           GHCJS.DOM
import           GHCJS.DOM.EventM              (on)
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.History
import           GHCJS.DOM.Location            (getPathname, getSearch)
import           GHCJS.DOM.PopStateEvent
import           GHCJS.DOM.Window
import           Language.Javascript.JSaddle
import           Servant.API                   hiding (uriPath, uriQuery)
import           Servant.Links                 (Link, URI (..), linkURI,
                                                safeLink)
import           Web.HttpApiData               (parseQueryParamMaybe,
                                                parseUrlPieceMaybe)

import           Shpadoinkle


default (Text)


-- #ifdef ghcjs_HOST_OS
-- foreign import javascript unsafe "window.foo($1)" dispatchIt :: JSVal -> JSM ()
-- #else
-- dispatchIt :: JSVal -> JSM ()
-- dispatchIt = undefined
-- #endif


-- | Term level API representation
data Router a where
  RChoice      :: Router a -> Router a -> Router a
  RCapture     :: FromHttpApiData x => (x -> Router a) -> Router a
  RQueryParam  :: (FromHttpApiData x, KnownSymbol sym) => Proxy sym -> (Maybe x -> Router a) -> Router a
  RQueryParams :: (FromHttpApiData x, KnownSymbol sym) => Proxy sym -> ([x] -> Router a) -> Router a
  RQueryFlag   :: KnownSymbol sym => Proxy sym -> (Bool -> Router a) -> Router a
  RPath        :: KnownSymbol sym => Proxy sym -> Router a -> Router a
  RView        :: a -> Router a


-- | Redirect is an existentialized Proxy that must be a member of the API
data Redirect api
  = forall sub. (IsElem sub api, HasLink sub)
  => Redirect (Proxy sub) (MkLink sub Link -> Link)


-- | Ensure global coherence between routes and the api
class Routed a r where redirect :: r -> Redirect a


navigate :: forall a m r. MonadJSM m => Routed a r => r -> m ()
navigate r = do
  w <- currentWindowUnchecked
  history <- getHistory w
  case redirect r :: Redirect a of
    Redirect pr mf -> do
      let uri = linkURI . mf $ safeLink (Proxy @a) pr
      pushState history () "" . Just . T.pack $
        "/" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
      e <- newPopStateEvent "popstate" Nothing
      _ <- dispatchEvent_ w e
      liftJSM GHCJS.DOM.syncPoint


fullPageSPA :: forall layout b a r m
   . HasRouter layout
  => Backend b m a
  => Eq a
  => (m ~> JSM)
  -- ^ how do we get to JSM?
  -> (TVar a -> b m ~> m)
  -- ^ What backend are we running?
  -> (r -> a)
  -- ^ what is the initial state?
  -> (a -> Html (b m) a)
  -- ^ how should the html look?
  -> b m RawNode
  -- ^ where do we render?
  -> (r -> a -> m a)
  -- ^ listen for route changes
  -> layout `RoutedAs` r
  -- ^ how shall we relate urls to routes?
  -> JSM ()
fullPageSPA toJSM backend i' view getStage onRoute routes = do
  let router = route @layout @r routes
  window <- currentWindowUnchecked
  getRoute window router $ \case
    Nothing -> return ()
    Just r -> do
      let i = i' r
      model <- createTerritory i
      _ <- listenPopState router $ writeUpdate model . (toJSM .) . onRoute
      shpadoinkle toJSM backend i model view getStage


-- | ?foo=bar&baz=qux -> [("foo","bar"),("baz","qux")]
parseQuery :: Text -> [(Text,Text)]
parseQuery =  (=<<) toKVs . T.splitOn "&" . T.drop 1
  where toKVs t = case T.splitOn "=" t of
                    [k,v] -> [(k,v)]
                    _     -> []


-- | /foo/bar -> ["foo","bar"]
parseSegments :: Text -> [Text]
parseSegments = C.filter (/= "") .  T.splitOn "/"


popstate :: EventName Window PopStateEvent
popstate = unsafeEventName "popstate"


getRoute
  :: Window -> Router r -> (Maybe r -> JSM a) -> JSM a
getRoute window router handle = do
  location <- getLocation window
  path     <- getPathname location
  search   <- getSearch location
  let query = parseQuery search
      segs  = parseSegments path
  handle $ fromRouter query segs router


listenPopState
  :: Router r -> (r -> JSM ()) -> JSM (JSM ())
listenPopState router handle = do
  window  <- currentWindowUnchecked
  window `on` popstate $ liftJSM . getRoute window router $ maybe (return ()) handle


-- | Get an @r@ from a route and url context
fromRouter :: [(Text,Text)] -> [Text] -> Router r -> Maybe r
fromRouter queries segs = \case
    RChoice x y        -> fromRouter queries segs x <|> fromRouter queries segs y
    RCapture f         -> case segs of
        []                 -> Nothing
        capture:paths      -> fromRouter queries paths . f =<< parseUrlPieceMaybe capture
    RQueryParam sym f  ->
        case lookup (T.pack $ symbolVal sym) queries of
            Nothing -> fromRouter queries segs $ f Nothing
            Just t  -> fromRouter queries segs $ f (parseQueryParamMaybe t)
    RQueryParams sym f ->
        fromRouter queries segs . f . compact $ parseQueryParamMaybe . snd <$> C.filter
            (\(k, _) -> k == T.pack (symbolVal sym))
            queries
    RQueryFlag sym f ->
        fromRouter queries segs . f . isJust $ lookup (T.pack $ symbolVal sym) queries
    RPath sym a        -> case segs of
        []                 -> Nothing
        p:paths            -> if p == T.pack (symbolVal sym) then
            fromRouter queries paths a else Nothing
    RView a            -> if null segs then Just a else Nothing


-- | This type class traverses the Servant API, and sets up a function to
-- build its term level representation.
class HasRouter layout where
    -- | RoutedAs should be surjective.
    -- As in one route can be the handler for more than one url.
    type RoutedAs layout route :: Type
    route :: RoutedAs layout route -> Router route

instance (HasRouter x, HasRouter y) => HasRouter (x :<|> y) where
    type RoutedAs (x :<|> y) r = RoutedAs x r :<|> RoutedAs y r
    route (x :<|> y) = RChoice (route @x x) (route @y y)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x)
    => HasRouter (Capture sym x :> sub) where
    type RoutedAs (Capture sym x :> sub) a = x -> RoutedAs sub a
    route = RCapture . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParam sym x :> sub) where
    type RoutedAs (QueryParam sym x :> sub) a = Maybe x -> RoutedAs sub a
    route = RQueryParam (Proxy @sym) . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParams sym x :> sub) where
    type RoutedAs (QueryParams sym x :> sub) a = [x] -> RoutedAs sub a
    route = RQueryParams (Proxy @sym) . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, KnownSymbol sym)
    => HasRouter (QueryFlag sym :> sub) where
    type RoutedAs (QueryFlag sym :> sub) a = Bool -> RoutedAs sub a
    route = RQueryFlag (Proxy @sym) . (route @sub .)

instance (HasRouter sub, KnownSymbol path)
    => HasRouter (path :> sub) where
    type RoutedAs (path :> sub) a = RoutedAs sub a
    route = RPath (Proxy @path) . route @sub
    {-# INLINABLE route #-}

instance HasRouter Raw where
    type RoutedAs Raw a = a
    route = RView
    {-# INLINABLE route #-}

