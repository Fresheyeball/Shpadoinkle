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
import           Shpadoinkle.JSFFI   (JSObject, fromJSValUnsafe, getProp,
                                      global, jsTreq, jsValToMaybeString,
                                      mkEmptyObject, mkFun', purely, setProp,
                                      toJSString, toJSVal, (#))
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
  o & setProp "type" (purely toJSVal $ "shpadoinkle_output_state")
  o & setProp "msg" (purely toJSVal $ purely toJSString $ show x)
  global # "postMessage" $ (o, "*")


listenForSetState :: forall a. Read a => TVar a -> JSM ()
listenForSetState model =
  void $ (global # "addEventListener") . ("message",) =<< (mkFun' $ \args -> do
    let e = fromJSValUnsafe @JSObject $ Prelude.head args
    isWindow <- (`jsTreq` global) <$> getProp "source" e
    d <- fromJSValUnsafe @JSObject <$> getProp "data" e
    isRightType <- (`jsTreq` (purely toJSVal "shpadoinkle_set_state")) <$> getProp "type" d
    msg <- jsValToMaybeString <$> getProp "msg" d
    case msg of
      Just msg' | isWindow && isRightType ->
        atomically . writeTVar model $ read msg'
      _ -> return ())

#else
withDeveloperTools :: forall a. Eq a => Read a => Show a => TVar a -> JSM ()
withDeveloperTools = const $ pure ()
#endif
