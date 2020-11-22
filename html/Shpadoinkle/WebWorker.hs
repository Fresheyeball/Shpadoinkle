{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}


module Shpadoinkle.WebWorker where


import           Control.Monad               (void)
import           Data.Text
import           GHCJS.DOM
import           Language.Javascript.JSaddle
import           Text.RawString.QQ


newtype Worker = Worker { unWorker :: JSVal }
  deriving (ToJSVal)


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


createWorker :: MonadJSM m => Text -> m Worker
createWorker url = liftJSM $ do
  _ <- eval createWorkerJS
  w <- toJSVal =<< currentWindowUnchecked
  u <- toJSVal url
  Worker <$> (w # ("createWorker" :: Text) $ [u])


postMessage :: ToJSVal a => MonadJSM m => Worker -> a -> m ()
postMessage (Worker worker) msg = liftJSM $ do
  v <- toJSVal msg
  () <$ (worker # ("postMessage" :: Text) $ [v])


postMessage' :: ToJSVal a => MonadJSM m => a -> m ()
postMessage' msg = liftJSM $ do
  self <- jsg ("self" :: Text)
  m <- toJSVal msg
  () <$ (self # ("postMessage" :: Text) $ m)


hackWindow :: MonadJSM m => m ()
hackWindow = void . liftJSM $ eval ("window = self" :: Text)


onMessage :: ToJSVal mailbox => FromJSVal message => MonadJSM m => mailbox -> (Maybe message -> JSM ()) -> m ()
onMessage mailbox f = liftJSM $ do
  box <- toJSVal mailbox
  (box <# ("onmessage" :: Text)) =<< toJSVal (fun (\_ _ -> \case
    [v] -> f =<< fromJSVal =<< (v ! ("data" :: Text))
    _   -> return ()))

