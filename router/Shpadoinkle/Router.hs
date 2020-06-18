{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


-- | This module provides for Servant based routing for Shpadoinkle applications.
-- The technique in use is standard for Servant. We have a GADT @Router@, and some
-- type class inductive programming with class @HasRouter@. The @Router@ the term
-- necissary for the runtime operation of single page application routing.
--
-- State changes are tracked by the "popstate" event and an @MVar ()@. Ideally this is
-- done the browser's native api's only, and not an @MVar@. However that approach is
-- blocked by a but in GHCjs which is documented here https://stackoverflow.com/questions/59954787/cant-get-dispatchevent-to-fire-in-ghcjs.


module Shpadoinkle.Router
    ( Raw, Redirect(..)
    , fromRouter
    , HasRouter(..)
    , HasLink(..)
    , MonadJSM
    , Routed(..)
    , fullPageSPA
    , navigate
    , withHydration
    , toHydration
    ) where


import           Control.Applicative
import           Control.Compactable           as C
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy          (fromStrict, toStrict)
import           Data.Kind
import           Data.Maybe                    (isJust)
import           Data.Proxy
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           GHC.TypeLits
import           GHCJS.DOM
import           GHCJS.DOM.EventM              (on)
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.History
import           GHCJS.DOM.Location            (getPathname, getSearch)
import           GHCJS.DOM.PopStateEvent
import           GHCJS.DOM.Window
import           Language.Javascript.JSaddle   (JSM, MonadJSM, fromJSVal, jsg, liftJSM)
import           Servant.API                   hiding (uriPath, uriQuery)
import           Servant.Links                 (Link, URI (..), linkURI,
                                                safeLink)
import           System.IO.Unsafe
import           UnliftIO.Concurrent           (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import           UnliftIO.STM                  (TVar, newTVarIO)
import           Web.HttpApiData               (parseQueryParamMaybe,
                                                parseUrlPieceMaybe)

import           Shpadoinkle


default (Text)


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


syncRoute :: MVar ()
syncRoute = unsafePerformIO newEmptyMVar
{-# NOINLINE syncRoute #-}


-- | When using serverside rendering you may benefit from seeding the page with
-- data. This function get an assumed global variable on the page called "initState"
-- if it's found, we return that. Otherwise we use the provided (r -> m a) function
-- to generate the init state for our app, based on the currout route. Typically
-- this is used on the client side.
withHydration :: (MonadJSM m, FromJSON a) => (r -> m a) -> r -> m a
withHydration s r = do
  i <- liftJSM $ fromJSVal =<< jsg "initState"
  case decode . fromStrict . encodeUtf8 =<< i of
    Just fe -> return fe
    _       -> s r

-- | When using serverside rendering you may benefit from seeding the page with
-- data. This function returns a script tag that makes a global variable "initState"
-- containing a JSON representation to be used as the initial state of the application
-- on page load. Typically this is used on the server side.
toHydration :: ToJSON a => IsHtml h p => a -> h b
toHydration fe =
  h "script" [] [ text . decodeUtf8 . toStrict $ "window.initState = '" <> encode fe <> "'" ]


-- | Change the browser's URL to the canonical URL for a given route `r`.
navigate :: forall a m r. MonadJSM m => Routed a r => r -> m ()
navigate r = do
  w <- currentWindowUnchecked
  history <- getHistory w
  case redirect r :: Redirect a of
    Redirect pr mf -> do
      let uri = linkURI . mf $ safeLink (Proxy @a) pr
      pushState history () "" . Just . T.pack $
        "/" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
      liftIO $ putMVar syncRoute ()


-- | This method wraps @shpadoinkle@ providing for a convenient entrypoint
-- for single page applications. It wires together your normal @shpadoinkle@
-- app components with a function to respond to route changes, and the route mapping
-- itself.
fullPageSPA :: forall layout b a r m
   . HasRouter layout
  => Backend b m a
  => Eq a
  => Functor m
  => (m ~> JSM)
  -- ^ how do we get to JSM?
  -> (TVar a -> b m ~> m)
  -- ^ What backend are we running?
  -> (r -> m a)
  -- ^ what is the initial state?
  -> (a -> HtmlM (b m) a)
  -- ^ how should the html look?
  -> b m RawNode
  -- ^ where do we render?
  -> (r -> m (Continuation m a))
  -- ^ listen for route changes
  -> layout :>> r
  -- ^ how shall we relate urls to routes?
  -> JSM ()
fullPageSPA toJSM backend i' view getStage onRoute routes = do
  let router = route @layout @r routes
  window <- currentWindowUnchecked
  getRoute window router $ \case
    Nothing -> return ()
    Just r -> do
      i <- toJSM $ i' r
      model <- newTVarIO i
      _ <- listenStateChange router $ writeUpdate model . Continuation . (id,) . const
           . (fmap (convertC toJSM) . toJSM) . onRoute
      shpadoinkle toJSM backend i model view getStage
      syncPoint


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


listenStateChange
  :: Router r -> (r -> JSM ()) -> JSM ()
listenStateChange router handle = do
  w <- currentWindowUnchecked
  _ <- on w popstate . liftIO $ putMVar syncRoute ()
  _ <- forkIO . forever $ do
    liftIO $ takeMVar syncRoute
    getRoute w router $ maybe (return ()) handle
  return ()


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
    -- | :>> (pronounced "routed as") should be surjective.
    -- As in one route can be the handler for more than one url.
    type layout :>> route :: Type
    route :: layout :>> route -> Router route


infixr 4 :>>


instance (HasRouter x, HasRouter y)
    => HasRouter (x :<|> y) where
    type (x :<|> y) :>> r = x :>> r :<|> y :>> r

    route :: x :>> r :<|> y :>> r -> Router r
    route (x :<|> y) = RChoice (route @x x) (route @y y)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x)
    => HasRouter (Capture sym x :> sub) where

    type (Capture sym x :> sub) :>> a = x -> sub :>> a

    route :: (x -> sub :>> r) -> Router r
    route = RCapture . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParam sym x :> sub) where

    type (QueryParam sym x :> sub) :>> a = Maybe x -> sub :>> a

    route :: (Maybe x -> sub :>> r) -> Router r
    route = RQueryParam (Proxy @sym) . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParams sym x :> sub) where

    type (QueryParams sym x :> sub) :>> a = [x] -> sub :>> a

    route :: ([x] -> sub :>> r) -> Router r
    route = RQueryParams (Proxy @sym) . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, KnownSymbol sym)
    => HasRouter (QueryFlag sym :> sub) where

    type (QueryFlag sym :> sub) :>> a = Bool -> sub :>> a

    route :: (Bool -> sub :>> r) -> Router r
    route = RQueryFlag (Proxy @sym) . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, KnownSymbol path)
    => HasRouter ((path :: Symbol) :> sub) where

    type (path :> sub) :>> a = sub :>> a

    route :: sub :>> r -> Router r
    route = RPath (Proxy @path) . route @sub
    {-# INLINABLE route #-}

instance HasRouter Raw where
    type Raw :>> a = a

    route :: r -> Router r
    route = RView
    {-# INLINABLE route #-}

