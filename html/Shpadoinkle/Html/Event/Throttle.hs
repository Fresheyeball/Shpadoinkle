{-# LANGUAGE FlexibleContexts #-}


module Shpadoinkle.Html.Event.Throttle
  ( throttleRaw
  , throttle
  , Throttle (..)
  ) where


import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Data.Time.Clock
import GHC.Conc
import Language.Javascript.JSaddle
import Shpadoinkle hiding (newTVarIO)
import Shpadoinkle.Html.Event.HandlerTransformer


newtype Throttle m a b = Throttle (HandlerTransformer m a b)


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



throttle :: IsProp p (Continuation m) => MonadIO n
         => NominalDiffTime
         -> n ( (b -> (Text, p a))
             ->  b -> (Text, p a) )
throttle duration = do
  f <- throttleRaw duration
  return $ \g x ->
    let (attr, p) = g x
    in (attr, cataProp textProp (listenerProp . f) flagProp p)
