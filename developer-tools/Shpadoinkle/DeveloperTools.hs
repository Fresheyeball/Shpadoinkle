{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes           #-}
#ifdef DEVELOPMENT
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
#endif
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Shpadoinkle.DeveloperTools (withDeveloperTools) where


import           Language.Javascript.JSaddle
import           UnliftIO
#ifdef DEVELOPMENT
import           Control.Lens
import           Control.Monad
import           Control.Monad.STM           (retry)
import           UnliftIO.Concurrent
#endif


default (JSString)


#ifdef DEVELOPMENT
withDeveloperTools :: forall a. Eq a => Read a => Show a => TVar a -> JSM ()
withDeveloperTools x = do
  i' <- readTVarIO x
  y  <- newTVarIO i'
  outputState i'
  syncPoint
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
  o <- obj
  (o <# "type") "shpadoinkle_output_state"
  (o <# "msg") $ toJSString $ show x
  jsg "window" ^. js2 "postMessage" o "*"


listenForSetState :: forall a. Read a => TVar a -> JSM ()
listenForSetState model = void $ jsg "window" ^. js2 "addEventListener" "message" (fun $ \_ _ args -> do
    let e = Prelude.head args
    isWindow <- strictEqual (e ^. js "source") (jsg "window")
    d <- e ^. js "data"
    isRightType <- strictEqual (d ^. js "type") "shpadoinkle_set_state"
    msg <- fromJSVal =<< (d ^. js "msg")
    case msg of
      Just msg' | isWindow && isRightType ->
        atomically . writeTVar model $ read msg'
      _ -> return ())

#else
withDeveloperTools :: forall a. Eq a => Read a => Show a => TVar a -> JSM ()
withDeveloperTools = const $ pure ()
#endif
