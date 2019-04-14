module Data.Once (Once, newOnce, runOnce) where


import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class


newtype Once m a = Once (MVar (m a))


newOnce :: MonadIO m => m a -> m (Once m a)
newOnce = liftIO . fmap Once . newMVar


runOnce :: MonadIO m => Once m a -> m a
runOnce (Once io) = do
  x <- join (liftIO $ takeMVar io)
  x <$ liftIO (putMVar io (return x))
