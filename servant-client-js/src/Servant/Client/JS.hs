{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wunused-imports        #-}
{-# OPTIONS_GHC -Wincomplete-patterns   #-}


module Servant.Client.JS
  ( module Servant.Client.Core.Reexport
  , ClientEnv (..)
  , ClientM (..)
  , runClientM
  , client
  , withStreamingRequestJSM
  ) where


import           Control.Concurrent
import           Control.Monad                (forM_)
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Except
import           Data.Binary.Builder          (toLazyByteString)
import qualified Data.ByteString.Char8        as BS
#ifdef ghcjs_HOST_OS
import qualified Data.ByteString.Builder      as BB
import           Data.Int                     (Int8)
#endif
import qualified Data.ByteString.Lazy         as BL
import           Data.CaseInsensitive
import           Data.Function                ((&))
import           Data.Functor.Alt
import           Data.Maybe                   (fromMaybe)
import           Data.Proxy
import qualified Data.Sequence                as Seq
import           Data.Text
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           GHC.Conc
import           GHC.Generics
#ifdef ghcjs_HOST_OS
import           GHCJS.Marshal.Internal
#endif
import           Shpadoinkle.JSFFI            (JSArray, JSM (..), JSObject,
                                               JSString, JSVal, consoleLog,
                                               downcast, getProp, getPropMaybe,
                                               ghcjsOnly, global, jsAs, jsTo,
                                               liftJSM, mkEmptyObject, mkFun,
                                               setProp, toBoolLax, (#))
#ifdef ghcjs_HOST_OS
import           GHCJS.Prim                   hiding (fromJSString, getProp)
#endif
import           Network.HTTP.Media           (renderHeader)
import           Network.HTTP.Types
import           Servant.Client.Core
import           Servant.Client.Core.Reexport
import qualified Servant.Types.SourceT        as S

default (Text)

o ! k = getProp k o


newtype ClientEnv = ClientEnv { baseUrl :: BaseUrl }
  deriving (Eq, Show)

newtype ClientM a = ClientM
  { runClientM' :: ReaderT ClientEnv (ExceptT ClientError JSM) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , Generic, MonadReader ClientEnv, MonadError ClientError
           , MonadThrow, MonadCatch )

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

runClientM :: ClientM a -> ClientEnv -> JSM (Either ClientError a)
runClientM m env = runExceptT $ runReaderT (runClientM' m) env

instance MonadBase IO ClientM where
  liftBase = ClientM . liftBase

instance MonadBaseControl IO ClientM where
  type StM ClientM a = Either ClientError a

  liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . runClientM')))

  restoreM st = ClientM (restoreM st)

instance Alt ClientM where
  a <!> b = a `catchError` const b

instance RunClient ClientM where
  runRequest = fetch
  throwClientError = throwError

instance RunStreamingClient ClientM where
  withStreamingRequest req handler = withStreamingRequestJSM req (liftIO . handler)


unJSString :: JSString -> Text
unJSString = jsAs


getFetchArgs :: ClientEnv -> Request -> JSM [JSVal]
getFetchArgs (ClientEnv (BaseUrl urlScheme host port basePath))
          (Request reqPath reqQs reqBody reqAccept reqHdrs _reqVer reqMethod) = do
  let schemeStr :: Text
      schemeStr = case urlScheme of
                    Http -> "http://"
                    Https -> "https://"
  url <- text2jsval $ schemeStr <> pack host <> ":" <> pack (show port) <> pack basePath
                             <> decodeUtf8 (BL.toStrict (toLazyByteString reqPath))
                             <> (if Prelude.null reqQs then "" else "?" ) <> (intercalate "&"
                                        $ (\(k,v) -> decodeUtf8 k <> "="
                                                           <> maybe "" decodeUtf8 v)
                                         <$> Prelude.foldr (:) [] reqQs)
  init <- mkEmptyObject
  methodStr <- text2jsval $ decodeUtf8 reqMethod
  init & setProp "method" methodStr
  headers <- mkEmptyObject
  forM_  reqHdrs $ \(original -> k, v) -> do
    v' <- text2jsval (decodeUtf8 v)
    headers & setProp (decodeUtf8 k) v'
  forM_ reqAccept $ \mt -> do
    mt' <- text2jsval (decodeUtf8 (renderHeader mt))
    headers & setProp "Accept" mt'
  init & setProp "headers" headers
  case reqBody of
    Just (RequestBodyLBS x, mt) -> do
      v <- text2jsval (decodeUtf8 (BL.toStrict x))
      init & setProp "body" v
      mt' <- text2jsval (decodeUtf8 (renderHeader mt))
      headers & setProp "Content-Type" mt'
    Just (RequestBodyBS x, mt) -> do
      v <- text2jsval (decodeUtf8 x)
      init & setProp "body" v
      mt' <- text2jsval (decodeUtf8 (renderHeader mt))
      headers & setProp "Content-Type" mt'
    Just (RequestBodySource _, _) -> error "Servant.Client.JS.withStreamingRequest(JSM) does not (yet) support RequestBodySource"
    Nothing -> return ()
  init' <- pure . jsAs @JSVal $ init
  return [url, init']

  where

  text2jsval :: Text -> JSM JSVal
  text2jsval = pure . jsAs


getResponseMeta :: JSVal -> JSM (Status, Seq.Seq Header, HttpVersion)
getResponseMeta res = do
  res' <- jsTo @JSObject res
  status <- toEnum . fromMaybe 200
            <$> (getPropMaybe ("status" :: Text) res')
  resHeadersObj :: JSObject <- res' ! ("headers" :: Text)
  resHeaderNames <- (resHeadersObj # ("keys" :: Text) $ ([] :: [JSVal]))
    >>= fix (\go names ->
      do x <- jsTo @JSObject names >>= (\n -> n # ("next" :: Text) $ ([] :: [JSVal]))
         x' <- jsTo @JSObject x
         isDone <- getPropMaybe ("done" :: Text) x'
         if isDone == Just True || isDone == Nothing
           then return []
           else do
             rest <- go names
             v <- getPropMaybe "value" x'
             case v of
               Just k -> return (k : rest)
               Nothing -> return rest)
  resHeaders <- fmap (Prelude.foldr (Seq.:<|) Seq.Empty)
             .  forM resHeaderNames $ \headerName -> do
    headerValue <- fmap (fromMaybe "" . (downcast :: JSVal -> Maybe Text))
                   $ (resHeadersObj # ("get" :: Text) $ [headerName])
    return (mk (encodeUtf8 (unJSString headerName)), encodeUtf8 headerValue)
  return (status, resHeaders, http11) -- http11 is made up


uint8arrayToByteString :: JSVal -> JSM BS.ByteString
#ifdef ghcjs_HOST_OS
uint8arrayToByteString ar = do
  _Array :: JSObject <- getProp "Array" global
  asArray :: [JSVal] <- pure . jsAs =<< jsTo @JSArray =<< (_Array # "from" $ ar)
  let asInt8s = asInt8Unchecked <$> asArray
  pure . BL.toStrict . BB.toLazyByteString $ foldMap BB.int8 asInt8s

foreign import javascript unsafe
  "$r = $1"  -- unchecked
  asInt8Unchecked :: JSVal -> Int8
#else
uint8arrayToByteString = ghcjsOnly
#endif


parseChunk :: JSVal -> JSM (Maybe BS.ByteString)
parseChunk chunk = do
  chunk' <- jsTo @JSObject chunk
  isDone <- toBoolLax
              =<< getProp ("done" :: Text) chunk'
  case isDone of
    True -> return Nothing
    False -> pure . Just =<< uint8arrayToByteString =<< getProp ("value" :: Text) chunk'


fetch :: Request -> ClientM Response
fetch req = ClientM . ReaderT $ \env -> do
  self :: JSObject <- liftJSM $ getProp ("self" :: Text) global
  args <- liftJSM $ getFetchArgs env req
  promise :: JSVal <- liftJSM $ self # ("fetch" :: Text) $ args
  promise' <- jsTo @JSObject promise
  contents <- liftIO $ newTVarIO (mempty :: BS.ByteString)
  result <- liftIO newEmptyMVar
  promiseHandler <- liftJSM $ mkFun (\_ _ args -> do
    case args of
      [res] -> do
        meta <- getResponseMeta res
        stream :: JSObject <- getProp ("body" :: Text) =<< jsTo @JSObject res
        rdr <- stream # ("getReader" :: Text) $ ([] :: [JSVal])
        rdr' <- jsTo @JSObject rdr
        _ <- fix $ \go -> do
          rdrPromise <- rdr' # ("read" :: Text) $ ([] :: [JSVal])
          rdrPromise' <- jsTo @JSObject rdrPromise
          rdrHandler <- mkFun (\_ _ args -> do
            case args of
              [chunk] -> do
                next <- parseChunk chunk
                case next of
                  Nothing -> liftIO $ putMVar result . (meta,) =<< readTVarIO contents
                  Just x -> do
                    liftIO . atomically $ writeTVar contents . (<> x) =<< readTVar contents
                    go
              _ -> do
                error "fetch read promise handler received wrong number of arguments")
          _ <- rdrPromise' # ("then" :: Text) $ [rdrHandler]
          return ()
        return ()
      _ -> error "fetch promise handler received wrong number of arguments")
  liftJSM $ promise' # ("then" :: Text) $ [promiseHandler]
  ((status, hdrs, ver), body) <- liftIO $ takeMVar result
  return $ Response status hdrs ver (BL.fromStrict body)


-- | A variation on @Servant.Client.Core.withStreamingRequest@ where the continuation / callback
--   passed as the second argument is in the JSM monad as opposed to the IO monad.
--   Executes the given request and passes the response data stream to the provided continuation / callback.
withStreamingRequestJSM :: Request -> (StreamingResponse -> JSM a) -> ClientM a
withStreamingRequestJSM req handler =
  ClientM . ReaderT $ \env -> do
    self :: JSObject <- liftJSM $ getProp "self" global
    fetchArgs <- liftJSM $ getFetchArgs env req
    fetchPromise <- liftJSM $ self # "fetch" $ fetchArgs
    fetchPromise' <- jsTo @JSObject fetchPromise
    push <- liftIO newEmptyMVar
    result <- liftIO newEmptyMVar
    fetchPromiseHandler <- liftJSM $ mkFun (\_ _ args ->
      case args of
        [res] -> do
          (status, hdrs, ver) <- getResponseMeta res
          res' <- jsTo @JSObject res
          stream :: JSVal <- res' ! ("body" :: Text)
          stream' <- jsTo @JSObject stream
          rdr <- stream' # ("getReader" :: Text) $ ([] :: [JSVal])
          rdr' <- jsTo @JSObject rdr
          _ <- fix $ \go -> do
            rdrPromise <- rdr' # ("read" :: Text) $ ([] :: [JSVal])
            rdrPromise' <- jsTo @JSObject rdrPromise
            rdrHandler <- mkFun (\_ _ args ->
              case args of
                [chunk] -> do
                  next <- parseChunk chunk
                  case next of
                    Just bs -> do
                      liftIO $ putMVar push (Just bs)
                      go
                    Nothing -> liftIO $ putMVar push Nothing
                _ -> error "wrong number of arguments to rdrHandler")
            _ <- rdrPromise' # ("then" :: Text) $ [rdrHandler]
            return ()
          let out :: forall b. (S.StepT IO BS.ByteString -> IO b) -> IO b
              out handler' = handler' .  S.Effect . fix $ \go -> do
                next <- takeMVar push
                case next of
                  Nothing -> return S.Stop
                  Just x -> return $ S.Yield x (S.Effect go)
          liftIO . putMVar result . Response status hdrs ver $ S.SourceT @IO out
        _ -> error "wrong number of arguments to Promise.then() callback")
    liftJSM $ fetchPromise' # "then" $ [fetchPromiseHandler]
    liftJSM . handler =<< liftIO (takeMVar result)
