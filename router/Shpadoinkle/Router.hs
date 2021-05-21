{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
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


-- | This module provides for Servant-based routing for Shpadoinkle applications.
-- The technique in use is standard for Servant. We have a GADT 'Router' and some
-- type class inductive programming with class 'HasRouter'. The 'Router' the term
-- necessary for the runtime operation of single page application routing.
--
-- State changes are tracked by the "popstate" event and an @MVar ()@. Ideally this is
-- done via the browser's native APIs only and not an 'MVar', however that approach is
-- blocked by a bug in GHCjs which is documented <https://stackoverflow.com/questions/59954787/cant-get-dispatchevent-to-fire-in-ghcjs here>.


module Shpadoinkle.Router (
    -- * Classes
    HasRouter(..), Routed(..)
    -- * Types
    , Redirect(..), Router(..), View, HTML
    -- * Shpadoinkle with SPA
    , fullPageSPAC, fullPageSPA, fullPageSPA'
    -- * Navigation
    , navigate, getURI
    -- * Rehydration
    , withHydration, toHydration
    -- * Re-Exports
    , Raw, S.RawM', S.RawM, MonadJSM, HasLink(..)
    -- * Sub route utilities
    , TraverseUnions (..), mapUnions
    ) where


import           Control.Applicative           (Alternative ((<|>)))
import           Control.Compactable           as C (Compactable (compact, filter))
import           Control.Monad                 (forever)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Aeson                    (FromJSON, ToJSON, decode,
                                                encode)
import           Data.ByteString.Lazy          (fromStrict, toStrict)
import           Data.Functor.Identity         (Identity (..))
import           Data.Kind                     (Type)
import           Data.Maybe                    (isJust)
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           GHC.TypeLits                  (KnownSymbol, Symbol, symbolVal)
import           GHCJS.DOM                     (currentWindowUnchecked,
                                                syncPoint)
import           GHCJS.DOM.EventM              (on)
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           GHCJS.DOM.History             (pushState)
import           GHCJS.DOM.Location            (getPathname, getSearch)
import           GHCJS.DOM.PopStateEvent       (PopStateEvent)
import           GHCJS.DOM.Types               (JSM, MonadJSM, liftJSM)
import           GHCJS.DOM.Window              (Window, getHistory, getLocation,
                                                scrollTo)
import           Language.Javascript.JSaddle   (fromJSVal, jsg)
#ifndef ghcjs_HOST_OS
import           Servant.API                   (Accept (contentTypes), Capture,
                                                FromHttpApiData, HasLink (..),
                                                IsElem, MimeRender (..),
                                                QueryFlag, QueryParam,
                                                QueryParam', QueryParams, Raw,
                                                Required, parseQueryParam,
                                                type (:<|>) (..), type (:>))
#else
import           Servant.API                   (Capture, FromHttpApiData,
                                                HasLink (..), IsElem, QueryFlag,
                                                QueryParam, QueryParam',
                                                QueryParams, Raw, Required,
                                                parseQueryParam,
                                                type (:<|>) (..), type (:>))
#endif
import           Servant.Links                 (Link, URI (..), linkURI,
                                                safeLink)
import qualified Servant.RawM                  as S
import           System.IO.Unsafe              (unsafePerformIO)
import           UnliftIO.Concurrent           (MVar, forkIO, newEmptyMVar,
                                                putMVar, takeMVar)
import           UnliftIO.STM                  (TVar, atomically, newTVarIO,
                                                writeTVar)
import           Web.HttpApiData               (parseUrlPiece)

import           Shpadoinkle                   (Backend, Continuation, Html,
                                                NFData, RawNode, h, hoist,
                                                kleisli, pur, shpadoinkle, text,
                                                type (~>), writeUpdate)

#ifndef ghcjs_HOST_OS


import qualified Data.ByteString.Lazy          as BSL
import qualified Data.List.NonEmpty            as NE
import qualified Network.HTTP.Media            as M
import qualified Servant                       as S
import           Servant.RawM.Server           ()

import           Shpadoinkle.Backend.Static    (renderStatic)


#endif


default (Text)


-- | Term level API representation
data Router a where
  RChoice      :: Router a -> Router a -> Router a
  RCapture     :: (Text -> Either Text x) -> (x -> Router a) -> Router a
  RQueryParam  :: KnownSymbol sym => (Text -> Either Text x) -> Proxy sym -> (Maybe x -> Router a) -> Router a
  RQueryParamR :: KnownSymbol sym => (Text -> Either Text x) -> Proxy sym -> (x -> Router a) -> Router a
  RQueryParams :: KnownSymbol sym => (Text -> Either Text x) -> Proxy sym -> ([x] -> Router a) -> Router a
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


-- | When using server-side rendering you may benefit from seeding the page with
-- data. This function get an assumed global variable on the page called "initState".
-- If it's found, we return that, otherwise we use the provided @(r -> m a)@ function
-- to generate the init state for our app, based on the current route. Typically
-- this is used on the client side.
withHydration :: (MonadJSM m, FromJSON a) => (r -> m a) -> r -> m a
withHydration s r = do
  i <- liftJSM $ fromJSVal =<< jsg "initState"
  case decode . fromStrict . encodeUtf8 =<< i of
    Just fe -> return fe
    _       -> s r


-- | When using server-side rendering, you may benefit from seeding the page with
-- data. This function returns a script tag that makes a global variable "initState"
-- containing a JSON representation to be used as the initial state of the application
-- on page load. Typically this is used on the server side.
toHydration :: ToJSON a => a -> Html m b
toHydration fe =
  h "script" [] [ text $ "window.initState = '" <> (T.replace "'" "\\'" . decodeUtf8 . toStrict $ encode fe) <> "'" ]


-- | Change the browser's URL to the canonical URL for a given route `r`.
navigate :: forall a m r. (MonadJSM m, Routed a r) => r -> m ()
navigate r = do
  w <- currentWindowUnchecked
  history <- getHistory w
  let uri = getURI @a @r r
  pushState history () "" . Just . T.pack $
    "/" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
  liftIO $ putMVar syncRoute ()


-- | Get the cannonical URI for a given route
getURI :: forall a r. Routed a r => r -> URI
getURI r =
  case redirect r :: Redirect a of
    Redirect pr mf -> linkURI . mf $ safeLink (Proxy @a) pr



-- | This method wraps @shpadoinkle@, providing for a convenient entrypoint
-- for single page applications. It wires together your normal @shpadoinkle@
-- app components with a function to respond to route changes and the route mapping
-- itself. This flavor provides access to the full power of @Continuation@ in case you
-- need to handle in-flight updates.
fullPageSPAC :: forall layout b a r m
   . HasRouter layout
  => Backend b m a
  => Monad (b m)
  => Eq a
  => NFData a
  => Functor m
  => (m ~> JSM)
  -- ^ how do we get to JSM?
  -> (TVar a -> b m ~> m)
  -- ^ What backend are we running?
  -> (r -> m a)
  -- ^ what is the initial state?
  -> (a -> Html (b m) a)
  -- ^ how should the html look?
  -> b m RawNode
  -- ^ where do we render?
  -> (r -> m (Continuation m a))
  -- ^ listen for route changes
  -> layout :>> r
  -- ^ how shall we relate urls to routes?
  -> JSM ()
fullPageSPAC toJSM backend i' view getStage onRoute routes = do
  let router = route @layout @r routes
  window <- currentWindowUnchecked
  getRoute window router $ \case
    Nothing -> return ()
    Just r -> do
      i <- toJSM $ i' r
      model <- newTVarIO i
      _ <- listenStateChange router $ writeUpdate model . kleisli . const
           . (fmap (hoist toJSM) . toJSM) . onRoute
      shpadoinkle toJSM backend model view getStage
      syncPoint


-- | This method wraps @shpadoinkle@, providing for a convenient entrypoint
-- for single page applications. It wires together your normal @shpadoinkle@
-- app components with a function to respond to route changes and the route mapping
-- itself.
fullPageSPA :: forall layout b a r m
   . HasRouter layout
  => Backend b m a
  => Monad (b m)
  => Eq a
  => NFData a
  => Functor m
  => (m ~> JSM)
  -- ^ how do we get to JSM?
  -> (TVar a -> b m ~> m)
  -- ^ What backend are we running?
  -> (r -> m a)
  -- ^ what is the initial state?
  -> (a -> Html (b m) a)
  -- ^ how should the html look?
  -> b m RawNode
  -- ^ where do we render?
  -> (r -> m a)
  -- ^ listen for route changes
  -> layout :>> r
  -- ^ how shall we relate urls to routes?
  -> JSM ()
fullPageSPA a b c v g s = fullPageSPAC @layout a b c v g (fmap (pur . const) . s)


-- | This method wraps @shpadoinkle@ providing for a convenient entrypoint
-- for single page applications. It wires together your normal @shpadoinkle@
-- app components with a function to respond to route changes, and the route mapping
-- itself.
{-# ANN fullPageSPA' ("HLint: ignore Reduce duplication" :: String) #-}
fullPageSPA' :: forall layout b a r m
   . HasRouter layout
  => Backend b m a
  => Monad (b m)
  => Eq a
  => NFData a
  => Functor m
  => (m ~> JSM)
  -- ^ how do we get to JSM?
  -> (TVar a -> b m ~> m)
  -- ^ what backend are we running?
  -> TVar a
  -- ^ where do we store the state?
  -> (r -> m a)
  -- ^ what is the initial state?
  -> (a -> Html (b m) a)
  -- ^ how should the html look?
  -> b m RawNode
  -- ^ where do we render?
  -> (r -> m (Continuation m a))
  -- ^ listen for route changes
  -> layout :>> r
  -- ^ how shall we relate urls to routes?
  -> JSM ()
fullPageSPA' toJSM backend model i' view getStage onRoute routes = do
  let router = route @layout @r routes
  window <- currentWindowUnchecked
  getRoute window router $ \case
    Nothing -> return ()
    Just r -> do
      i <- toJSM $ i' r
      atomically $ writeTVar model i
      _ <- listenStateChange router $ writeUpdate model . kleisli . const
           . (fmap (hoist toJSM) . toJSM) . onRoute
      shpadoinkle toJSM backend model view getStage
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
    syncPoint
    scrollTo w 0 0
  return ()


-- | Get an @r@ from a route and url context
fromRouter :: [(Text,Text)] -> [Text] -> Router r -> Maybe r
fromRouter queries segs = \case
    RChoice x y                -> fromRouter queries segs x <|> fromRouter queries segs y
    RCapture decoder f         -> case segs of
        []            -> Nothing
        capture:paths -> fromRouter queries paths . f =<< mabify decoder capture
    RQueryParam decoder sym f  ->
        case lookup (T.pack $ symbolVal sym) queries of
            Nothing -> fromRouter queries segs $ f Nothing
            Just t  -> fromRouter queries segs $ f (mabify decoder t)
    RQueryParamR decoder sym f ->
       case lookup (T.pack $ symbolVal sym) queries of
            Nothing -> Nothing
            Just t  -> fromRouter queries segs . f =<< mabify decoder t
    RQueryParams decoder sym f ->
        fromRouter queries segs . f . compact $ mabify decoder . snd <$> C.filter
            (\(k, _) -> k == T.pack (symbolVal sym))
            queries
    RQueryFlag sym f ->
        fromRouter queries segs . f . isJust $ lookup (T.pack $ symbolVal sym) queries
    RPath sym a        -> case segs of
        []                 -> Nothing
        p:paths            -> if p == T.pack (symbolVal sym) then
            fromRouter queries paths a else Nothing
    RView a            -> if null segs then Just a else Nothing
  where
    mabify :: (x -> Either e a) -> (x -> Maybe a)
    mabify f input = case f input of
                      (Left _)  -> Nothing
                      (Right x) -> Just x


-- | This type class traverses the Servant API and sets up a function to
-- build its term level representation.
class HasRouter layout where
    -- | ':>>' (pronounced "routed as") should be surjective,
    -- as in one route can be the handler for more than one URL.
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
    route = RCapture parseUrlPiece . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParam sym x :> sub) where

    type (QueryParam sym x :> sub) :>> a = Maybe x -> sub :>> a

    route :: (Maybe x -> sub :>> r) -> Router r
    route = RQueryParam parseQueryParam (Proxy @sym) . (route @sub .)
    {-# INLINABLE route #-}

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
   => HasRouter (QueryParam' '[Required] sym x :> sub) where

  type (QueryParam' '[Required] sym x :> sub) :>> a = x -> sub :>> a

  route :: (x -> sub :>> r) -> Router r
  route = RQueryParamR parseQueryParam (Proxy @sym) . (route @sub .)

instance (HasRouter sub, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParams sym x :> sub) where

    type (QueryParams sym x :> sub) :>> a = [x] -> sub :>> a

    route :: ([x] -> sub :>> r) -> Router r
    route = RQueryParams parseQueryParam (Proxy @sym) . (route @sub .)
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

instance HasRouter (S.RawM' serverType) where
    type S.RawM' serverType :>> a = a

    route :: r -> Router r
    route = RView
    {-# INLINABLE route #-}

instance HasRouter (f '[HTML] (Html m b)) where
    type f '[HTML] (Html m b) :>> a = a

    route :: r -> Router r
    route = RView
    {-# INLINABLE route #-}

instance HasRouter (View m b) where
    type View m b :>> a = a

    route :: r -> Router r
    route = RView
    {-# INLINABLE route #-}


#ifndef ghcjs_HOST_OS


instance Accept HTML where
    contentTypes _ =
      "text" M.// "html" M./: ("charset", "utf-8") NE.:|
      ["text" M.// "html"]


instance MimeRender HTML (Html m a) where
  mimeRender _ =  BSL.fromStrict . encodeUtf8 . renderStatic


instance S.HasServer (View m a) context where
  type ServerT (View m a) n = S.ServerT S.RawM n
  route _                    = S.route                  (Proxy @S.RawM)
  hoistServerWithContext _   = S.hoistServerWithContext (Proxy @S.RawM)


#endif


-- | A Mime type for rendering Html as "text/html"
data HTML :: Type


-- | Servant terminal for Shpadoinkle views (recommended)
data View :: (Type -> Type) -> Type -> Type


instance HasLink (View m a) where
  type MkLink (View m a) b = b
  toLink toA _ = toA


type family SwitchOutput layout b :: Type where
  SwitchOutput ((a :<|> as) :<|> r) b = (b :<|> SwitchOutput as b) :<|> SwitchOutput r b
  SwitchOutput (a :<|> r) b = b :<|> SwitchOutput r b
  SwitchOutput a b = b


type family SwitchInput layout :: Type where
  SwitchInput ((a :<|> as) :<|> r) = a
  SwitchInput (a :<|> r) = a
  SwitchInput a = a


class TraverseUnions m layout b where
  traverseUnions :: (SwitchInput layout -> m b) -> layout -> m (SwitchOutput layout b)

instance
  ( Applicative m
  , TraverseUnions m r b
  , TraverseUnions m as b
  , SwitchInput r ~ a
  , SwitchInput as ~ a)
  => TraverseUnions m ((a :<|> as) :<|> r) b where
  traverseUnions f ((a :<|> as) :<|> r) = (\a' as' r' -> (a' :<|> as') :<|> r')
    <$> f a
    <*> traverseUnions @m @as @b f as
    <*> traverseUnions @m @r @b f r

instance
  {-# OVERLAPPABLE #-}
  ( Applicative m
  , TraverseUnions m r b
  , SwitchInput (a :<|> r) ~ a
  , SwitchInput r ~ a
  , SwitchOutput (a :<|> r) b ~ (b :<|> SwitchOutput r b))
  => TraverseUnions m (a :<|> r) b where
  traverseUnions f (a :<|> r) = (:<|>) <$> f a <*> traverseUnions @m @r @b f r


instance {-# OVERLAPPABLE #-} (SwitchOutput a b ~ b, SwitchInput a ~ a)
  => TraverseUnions m a b where
  traverseUnions f a = f a


mapUnions :: TraverseUnions Identity a b => (SwitchInput a -> b) -> a -> SwitchOutput a b
mapUnions f = runIdentity . traverseUnions (Identity . f)
