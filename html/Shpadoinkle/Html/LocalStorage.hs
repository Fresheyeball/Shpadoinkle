{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}


-- | Local storage IO operations
-- Get and set local storage values from some 'LocalStorageKey'


module Shpadoinkle.Html.LocalStorage where


import           Control.Monad       (void)
import           Data.Function       ((&))
import           Data.Maybe          (fromMaybe)
import           Data.String         (IsString)
import           Data.Text           (Text, unpack)
import           GHC.Generics        (Generic)
import           Shpadoinkle.JSFFI   (MonadJSM, getItem, jsAs, liftJSM,
                                      localStorage, setItem)
import           Text.Read           (readMaybe)
import           UnliftIO            (MonadIO (liftIO), MonadUnliftIO, TVar,
                                      newTVarIO)
import           UnliftIO.Concurrent (forkIO)

import           Shpadoinkle         (shouldUpdate)


-- | The key for a specific state kept in local storage
newtype LocalStorageKey a = LocalStorageKey { unLocalStorageKey :: Text }
  deriving (Semigroup, Monoid, IsString, Eq, Ord, Show, Read, Generic)


setStorage :: MonadJSM m => Show a => LocalStorageKey a -> a -> m ()
setStorage (LocalStorageKey k) m =
  localStorage & setItem k (show m)


getStorage :: MonadJSM m => Read a => LocalStorageKey a -> m (Maybe a)
getStorage (LocalStorageKey k) =
  (>>= (readMaybe . unpack . jsAs)) <$> (localStorage & getItem k)


-- When we should update we save
saveOnChange :: MonadJSM m => Show a => Eq a
             => LocalStorageKey a -> TVar a -> m ()
saveOnChange k = liftJSM . shouldUpdate (const $ setStorage k) ()


manageLocalStorage
  :: MonadUnliftIO m
  => Show a
  => Read a
  => Eq a
  => LocalStorageKey a -> a -> m (TVar a)
manageLocalStorage k initial = do
  model <- liftIO . newTVarIO . fromMaybe initial =<< getStorage k
  void . forkIO $ saveOnChange k model
  return model
