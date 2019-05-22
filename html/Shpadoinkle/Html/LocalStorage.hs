{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}


module Shpadoinkle.Html.LocalStorage where


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Maybe
import           Data.Monoid                 ((<>))
import           Data.String
import           Data.Text
import           Language.Javascript.JSaddle
import           Text.Read


newtype LocalStorageKey = LocalStorageKey { unLocalStorageKey :: Text }
  deriving (Semigroup, Monoid, IsString, Eq, Ord, Show, Read)


setStorage :: Show a => LocalStorageKey -> a -> IO ()
setStorage (LocalStorageKey k) m =
  void . eval $ "localStorage.setItem('" <> k <> "', " <> pack (show (show m)) <> ")"


getStorage :: Read a => LocalStorageKey -> IO (Maybe a)
getStorage (LocalStorageKey k) =
  fmap (readMaybe =<<) . fromJSVal =<< eval ("localStorage.getItem('" <> k <> "')")


saveOnChange :: Show a => Eq a => LocalStorageKey -> TVar a -> TVar a -> IO ()
saveOnChange k old new' = do
  n <- atomically $ do
    o <- readTVar old; n <- readTVar new'
    if o == n then retry else n <$ writeTVar old n
  setStorage k n
  saveOnChange k old new'


manageLocalStorage :: Show a => Read a => Eq a => LocalStorageKey -> a -> IO (TVar a)
manageLocalStorage k initial = do
  model <- newTVarIO . fromMaybe initial =<< getStorage k
  old   <- newTVarIO =<< readTVarIO model
  void . forkIO $ saveOnChange k old model
  return model
