{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}


module Shpadoinkle.Html.LocalStorage where


import           Control.Concurrent.STM      (retry)
import           Control.Monad
import           Data.Maybe
import           Data.Monoid                 ((<>))
import           Data.String
import           Data.Text
import           Language.Javascript.JSaddle
import           Text.Read
import           UnliftIO
import           UnliftIO.Concurrent         (forkIO)


newtype LocalStorageKey = LocalStorageKey { unLocalStorageKey :: Text }
  deriving (Semigroup, Monoid, IsString, Eq, Ord, Show, Read)


setStorage :: MonadJSM m => Show a => LocalStorageKey -> a -> m ()
setStorage (LocalStorageKey k) m =
  liftJSM . void . eval $ "localStorage.setItem('" <> k <> "', " <> pack (show (show m)) <> ")"


getStorage :: MonadJSM m => Read a => LocalStorageKey -> m (Maybe a)
getStorage (LocalStorageKey k) =
  liftJSM $ fmap (readMaybe =<<) . fromJSVal =<< eval ("localStorage.getItem('" <> k <> "')")


saveOnChange :: MonadJSM m => Show a => Eq a => LocalStorageKey -> TVar a -> TVar a -> m ()
saveOnChange k old new' = do
  n <- liftIO . atomically $ do
    o <- readTVar old; n <- readTVar new'
    if o == n then retry else n <$ writeTVar old n
  setStorage k n
  saveOnChange k old new'


manageLocalStorage
  :: MonadUnliftIO m
#ifndef ghcjs_HOST_OS
  => MonadJSM m
#endif
  => Show a
  => Read a
  => Eq a
  => LocalStorageKey -> a -> m (TVar a)
manageLocalStorage k initial = do
  model <- liftIO . newTVarIO . fromMaybe initial =<< getStorage k
  old   <- liftIO $ newTVarIO =<< readTVarIO model
  void . forkIO $ saveOnChange k old model
  return model
