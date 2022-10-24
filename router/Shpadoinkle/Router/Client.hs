{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-type-defaults              #-}


-- | Helper for querying the server from client side code using a derived client.
-- This module exists to save you from having to use CPP yourself.


module Shpadoinkle.Router.Client
  ( runXHR
  , runXHR'
  , runXHRe
  , getClientEnv
  , module Servant.Client.JS
  ) where


import           Control.Monad.Catch (MonadThrow (throwM))
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import           Servant.Client.JS   (BaseUrl (..), ClientEnv (..),
                                      ClientError (..), ClientM (..),
                                      EmptyClient (..), HasClient (..),
                                      InvalidBaseUrlException, Response,
                                      ResponseF (..), Scheme (..),
                                      StreamingResponse, client, parseBaseUrl,
                                      runClientM, showBaseUrl,
                                      withStreamingRequestJSM)
import           Shpadoinkle.JSFFI   (JSM, JSObject, getLocation, getPropMaybe)
import           Text.Read           (readMaybe)
import           UnliftIO            (MonadIO (liftIO))


default (Text)


getClientEnv :: JSM ClientEnv
getClientEnv = do
  loc :: JSObject <- getLocation
  protocol <- mapProtocol <$> (getPropMaybe ("protocol" :: Text) loc)
  hostname <- fromMaybe "localhost" <$> (getPropMaybe ("hostname" :: Text) loc)
  port <- fromMaybe (defaultPort protocol) . (readMaybe =<<) <$> (getPropMaybe ("port" :: Text) loc)
  return $ ClientEnv $ BaseUrl protocol hostname port ""

  where mapProtocol :: Maybe String -> Scheme
        mapProtocol (Just "https:") = Https
        mapProtocol _               = Http

        defaultPort :: Scheme -> Int
        defaultPort Https = 443
        defaultPort Http  = 80


-- | Run the ClientM from Servant as an XHR request.
runXHR :: ClientM a -> JSM a
runXHR m = runXHR' m =<< getClientEnv


-- | Run the ClientM from Servant as an XHR request with a customized base URL.
runXHR' :: ClientM a -> ClientEnv -> JSM a
runXHR' m env = either (liftIO . throwM) pure =<< runClientM m env


runXHRe :: ClientM a -> ClientEnv -> JSM (Either ClientError a)
runXHRe = runClientM
