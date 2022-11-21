{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes           #-}
#ifdef DEVELOPMENT
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
#endif
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Shpadoinkle.DeveloperTools (withDeveloperTools) where


import           Shpadoinkle.JSFFI   (JSM)
import           UnliftIO
#ifdef DEVELOPMENT
import           Control.Lens        hiding ((#))
import           Control.Monad
import           Control.Monad.STM   (retry)
import           Shpadoinkle.JSFFI   (JSObject, JSVal, getProp, getPropMaybe,
                                      jsTo, mkEmptyObject, mkFun', setProp,
                                      window, (#-), (===))
import           UnliftIO.Concurrent
#endif


#ifdef DEVELOPMENT
withDeveloperTools :: forall a. Eq a => Read a => Show a => TVar a -> JSM ()
withDeveloperTools x = do
  i' <- readTVarIO x
  y  <- newTVarIO i'
  outputState i'
  -- syncPoint
  listenForSetState x
  () <$ forkIO (f y)
  where
  f y = do
    x' <- atomically $ do
      y' :: a <- readTVar y
      x' :: a <- readTVar x
      if x' == y' then retry else x' <$ writeTVar y x'
    outputState x'
    f y


outputState :: forall a. Show a => a -> JSM ()
outputState x = void . (try :: forall b. JSM b -> JSM (Either SomeException b)) $ do
  o <- mkEmptyObject
  o & setProp "type" "shpadoinkle_output_state"
  o & setProp "msg" (show x)
  window #- "postMessage" $ (o, "*")


listenForSetState :: forall a. Read a => TVar a -> JSM ()
listenForSetState model =
  (window #- "addEventListener") . ("message",) =<< (mkFun' $ \args -> do
    e <- jsTo @JSObject $ Prelude.head args
    isWindow <- (=== window) <$> getProp @JSVal "source" e
    d :: Maybe JSObject <- getPropMaybe "data" e
    case d of
      Nothing -> pure ()
      Just d' -> do
        isRightType <- (=== "shpadoinkle_set_state") <$> getProp @JSVal "type" d'
        msg <- getPropMaybe "msg" d'
        case msg of
          Just msg' | isWindow && isRightType -> do
            atomically . writeTVar model $ read msg'
          _ -> return ())

#else
withDeveloperTools :: forall a. Eq a => Read a => Show a => TVar a -> JSM ()
withDeveloperTools = const $ pure ()
#endif
