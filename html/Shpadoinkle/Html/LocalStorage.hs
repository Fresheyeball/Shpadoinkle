{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}


-- | Local storage IO operations
-- Get and set localstorage values from some 'LocalStorageKey'


module Shpadoinkle.Html.LocalStorage where


import           Control.Monad
import           Data.Maybe
import           Data.String
import           Data.Text
import           GHC.Generics
import           GHCJS.DOM
import           GHCJS.DOM.Storage
import           GHCJS.DOM.Window
import           Text.Read
import           UnliftIO
import           UnliftIO.Concurrent (forkIO)

import           Shpadoinkle         (MonadJSM, liftJSM, shouldUpdate)


-- | The key for a specific state kept in local storage
newtype LocalStorageKey a = LocalStorageKey { unLocalStorageKey :: Text }
  deriving (Semigroup, Monoid, IsString, Eq, Ord, Show, Read, Generic)


setStorage :: MonadJSM m => Show a => LocalStorageKey a -> a -> m ()
setStorage (LocalStorageKey k) m = do
  s <- getLocalStorage =<< currentWindowUnchecked
  setItem s k $ show m


getStorage :: MonadJSM m => Read a => LocalStorageKey a -> m (Maybe a)
getStorage (LocalStorageKey k) = do
  s <- getLocalStorage =<< currentWindowUnchecked
  (>>= readMaybe) <$> getItem s k


-- Whe we should update we save
saveOnChange :: MonadJSM m => Show a => Eq a
             => LocalStorageKey a -> TVar a -> m ()
saveOnChange k = liftJSM . shouldUpdate (const $ setStorage k) ()


manageLocalStorage
  :: MonadUnliftIO m
#ifndef ghcjs_HOST_OS
  => MonadJSM m
#endif
  => Show a
  => Read a
  => Eq a
  => LocalStorageKey a -> a -> m (TVar a)
manageLocalStorage k initial = do
  model <- liftIO . newTVarIO . fromMaybe initial =<< getStorage k
  void . forkIO $ saveOnChange k model
  return model
