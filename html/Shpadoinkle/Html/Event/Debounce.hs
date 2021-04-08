{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module Shpadoinkle.Html.Event.Debounce
  ( debounce
  , debounceRaw
  , Debounce (..)
  ) where


import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Data.Time.Clock        (NominalDiffTime, getCurrentTime)
import           Shpadoinkle            (Continuation, JSM, MonadJSM, Prop,
                                         RawEvent, RawNode, atomically,
                                         bakedProp, cataProp, dataProp, done,
                                         flagProp, kleisli, liftJSM,
                                         listenerProp, newTVarIO, readTVar,
                                         textProp, writeTVar)
import           UnliftIO.Concurrent    (threadDelay)


newtype Debounce m a b = Debounce { runDebounce
  :: (a -> (Text, Prop m b))
  ->  a -> (Text, Prop m b) }


debounceRaw :: MonadJSM m => MonadIO n
            => NominalDiffTime
            -> n ( (RawNode -> RawEvent -> JSM (Continuation m a))
                ->  RawNode -> RawEvent -> JSM (Continuation m a) )
debounceRaw duration = do
  lastTriggered <- newTVarIO Nothing
  return $ \handler rn re -> do
    t0 <- liftIO getCurrentTime
    liftIO . atomically $ do
      t <- fromMaybe t0 <$> readTVar lastTriggered
      writeTVar lastTriggered (Just (max t t0))
    return . kleisli $ \_ -> do
      liftIO . threadDelay . truncate $ duration * 1000000
      continue <- liftIO . atomically $ do
        t1 <- readTVar lastTriggered
        return $ t1 == Just t0
      if continue then liftJSM $ handler rn re else return done


debounce :: MonadJSM m => MonadIO n
         => NominalDiffTime
         -> n (Debounce m a b)
debounce duration = do
  db <- debounceRaw duration
  return . Debounce $ \g x -> let (attr, p) = g x in (attr,
     cataProp dataProp textProp flagProp (listenerProp . db) bakedProp p)
