{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}


module Shpadoinkle.WebWorker where


import           Control.Monad     (void)
import           Data.Text
import           Shpadoinkle.JSFFI (JSM, JSObject, JSVal, MonadJSM, eval,
                                    getProp, global, jsAs, jsTo, liftJSM,
                                    mkFun', type (<:), window, (#), (#-))
import           Text.RawString.QQ


newtype Worker = Worker { unWorker :: JSObject }


createWorkerJS :: Text
createWorkerJS = [r|createWorker = function (workerUrl) {
  var worker = null;
  try {
    worker = new Worker(workerUrl);
  } catch (e) {
    try {
      var blob;
      try {
        blob = new Blob(["importScripts('" + workerUrl + "');"], { "type": 'application/javascript' });
      } catch (e1) {
        var blobBuilder = new (window.BlobBuilder || window.WebKitBlobBuilder || window.MozBlobBuilder)();
        blobBuilder.append("importScripts('" + workerUrl + "');");
        blob = blobBuilder.getBlob('application/javascript');
      }
      var url = window.URL || window.webkitURL;
      var blobUrl = url.createObjectURL(blob);
      worker = new Worker(blobUrl);
    } catch (e2) {
      //if it still fails, there is nothing much we can do
    }
  }
  return worker;
}|]


type ToJSVal a = (a <: JSVal)


createWorker :: MonadJSM m => Text -> m Worker
createWorker url = liftJSM $ do
  _ <- eval createWorkerJS
  Worker <$> (window # ("createWorker" :: Text) $ [jsAs @JSVal url])


postMessage :: (MonadJSM m, ToJSVal a) => Worker -> a -> m ()
postMessage (Worker worker) msg = liftJSM $ do
  worker #- ("postMessage" :: Text) $ [jsAs @JSVal msg]


postMessage' :: (MonadJSM m, ToJSVal a) => a -> m ()
postMessage' msg = liftJSM $ do
  self :: JSObject <- getProp ("self" :: Text) global
  self #- ("postMessage" :: Text) $ [jsAs @JSVal msg]


hackWindow :: MonadJSM m => m ()
hackWindow = void . liftJSM $ eval ("window = self" :: Text)


onMessage :: (MonadJSM m, mailbox <: JSObject) => mailbox -> (JSVal -> JSM ()) -> m ()
onMessage box f = do
  liftJSM $ do
    fun <- mkFun' $ \case
      [v] -> f =<< getProp ("data" :: Text) =<< jsTo @JSObject v
      _   -> return ()
    box #- ("onmessage" :: Text) $ [jsAs @JSVal fun]

