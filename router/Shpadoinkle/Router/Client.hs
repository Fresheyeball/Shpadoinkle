{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-type-defaults              #-}

-- | Helper for querying the server from client side code, using a derived client.
-- This module exists to save you from having to use CPP yourself.

module Shpadoinkle.Router.Client
  ( runXHR
  , runXHR'
  , module Servant.Client.JS
  ) where


import           Control.Monad (join)
import           Control.Monad.Catch
import           Data.Maybe
import           Data.Text
import           Language.Javascript.JSaddle hiding (JSM)
import           Servant.Client.JS
import           Text.Read
import           GHCJS.DOM.Types hiding (Text)
import           UnliftIO

default (Text)


-- | Run the ClientM from Servant as an XHR request.
runXHR :: ClientM a -> JSM a
runXHR m = do -- TODO cache the base url or make it optional
  loc <- jsg ("window" :: Text) >>= (! ("location" :: Text))
  protocol <- mapProtocol <$> (loc ! ("protocol" :: Text) >>= fromJSVal)
  hostname <- fromMaybe "localhost" <$> (loc ! ("hostname" :: Text) >>= fromJSVal)
  port <- fromMaybe (defaultPort protocol) . join . fmap readMaybe <$> (loc ! ("port" :: Text) >>= fromJSVal)
  runXHR' m . ClientEnv $ BaseUrl protocol hostname port ""
  where mapProtocol :: Maybe String -> Scheme
        mapProtocol (Just "https:") = Https
        mapProtocol _ = Http

        defaultPort :: Scheme -> Int
        defaultPort Https = 443
        defaultPort Http = 80

-- | Run the ClientM from Servant as an XHR request with a customized base URL.
runXHR' :: ClientM a -> ClientEnv -> JSM a
runXHR' m env = either (liftIO . throwM) pure =<< runClientM m env
