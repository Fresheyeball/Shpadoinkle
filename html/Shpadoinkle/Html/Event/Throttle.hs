{-# LANGUAGE FlexibleContexts #-}


module Shpadoinkle.Html.Event.Throttle
  ( throttle
  , Throttle
  , runThrottle
  ) where


import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text              (Text)
import           Data.Time.Clock        (NominalDiffTime, diffUTCTime,
                                         getCurrentTime)
import           Shpadoinkle            (Continuation, JSM, Prop, RawEvent,
                                         RawNode, atomically, bakedProp,
                                         cataProp, dataProp, done, flagProp,
                                         listenerProp, newTVarIO, readTVar,
                                         textProp, writeTVar)


newtype Throttle m a b = Throttle { runThrottle
  :: (a -> (Text, Prop m b))
  ->  a -> (Text, Prop m b) }


throttleRaw :: MonadIO n
            => NominalDiffTime
            -> n ( (RawNode -> RawEvent -> JSM (Continuation m a))
                ->  RawNode -> RawEvent -> JSM (Continuation m a) )
throttleRaw duration = do
  lastTriggered <- liftIO $ newTVarIO Nothing
  return $ \handler rn re -> do
    t1 <- liftIO getCurrentTime
    continue <- liftIO . atomically $ do
      t0m <- readTVar lastTriggered
      case t0m of
        Nothing -> do
          writeTVar lastTriggered (Just t1)
          return True
        Just t0 -> do
          let continue = diffUTCTime t1 t0 > duration
          when continue $ writeTVar lastTriggered (Just t1)
          return continue
    if continue then handler rn re else return done


throttle :: MonadIO n
         => NominalDiffTime
         -> n (Throttle m a b)
throttle duration = do
  f <- throttleRaw duration
  return . Throttle $ \g x ->
    let (attr, p) = g x
    in (attr, cataProp dataProp textProp flagProp (listenerProp . f) bakedProp p)
