{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}


module Shpadoinkle.Html.Event where


import           Control.Monad               (msum)
import           Data.Text
import           Language.Javascript.JSaddle

import           Shpadoinkle
import           Shpadoinkle.Html.TH


listenRaw :: Text -> (RawNode -> RawEvent -> m o) -> (Text, Prop m o)
listenRaw k = (,) k . PListener
listen :: Text -> m o -> (Text, Prop m o)
listen k = listenRaw k . const . const
listen' :: Applicative m => Text -> o -> (Text, Prop m o)
listen' k f = listen k $ pure f


onInput :: MonadJSM m => (Text -> m o) -> (Text, Prop m o)
onInput f = listenRaw "input" $ \(RawNode n) _ ->
  f =<< liftJSM (valToText =<< unsafeGetProp "value" =<< valToObject n)
onInput' :: MonadJSM m => (Text -> o) -> (Text, Prop m o)
onInput' f = onInput (pure . f)


type KeyCode = Int


mkOnKey :: MonadJSM m => Text -> (KeyCode -> m o) -> (Text, Prop m o)
mkOnKey t f = listenRaw t $ \_ (RawEvent e) ->
  f =<< liftJSM (fmap round $ valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)
onKeyup, onKeydown, onKeypress :: MonadJSM m => (KeyCode -> m o) -> (Text, Prop m o)
onKeyup    = mkOnKey "keyup"
onKeydown  = mkOnKey "keydown"
onKeypress = mkOnKey "keypress"
onKeyup', onKeydown', onKeypress' :: MonadJSM m => (KeyCode -> o) -> (Text, Prop m o)
onKeyup'    f = onKeyup    (pure . f)
onKeydown'  f = onKeydown  (pure . f)
onKeypress' f = onKeypress (pure . f)


onSubmit :: MonadJSM m => m o -> (Text, Prop m o)
onSubmit m = listenRaw "submit" $ \_ (RawEvent e) ->
  liftJSM (valToObject e # ("preventDefault" :: String) $ ([] :: [()])) >> m
onSubmit' :: MonadJSM m => o -> (Text, Prop m o)
onSubmit' = onSubmit . pure


$(msum <$> mapM mkEventDSL
  [ "click"
  , "contextmenu"
  , "dblclick"
  , "mousedown"
  , "mouseleave"
  , "mousemove"
  , "mouseover"
  , "mouseout"
  , "mouseup"
  , "beforeunload"
  , "error"
  , "hashchange"
  , "load"
  , "pageshow"
  , "pagehide"
  , "resize"
  , "scroll"
  , "unload"
  , "blur"
  , "change"
  , "focus"
  , "focusin"
  , "focusout"
  , "invalid"
  , "reset"
  , "search"
  , "select"
  , "drag"
  , "dragend"
  , "dragenter"
  , "dragleave"
  , "dragover"
  , "dragstart"
  , "drop"
  , "copy"
  , "cut"
  , "paste"
  , "afterprint"
  , "beforeprint"
  , "abort"
  , "canplay"
  , "canplaythrough"
  , "durationchange"
  , "emptied"
  , "ended"
  , "loadeddata"
  , "loadedmetadata"
  , "loadstart"
  , "pause"
  , "play"
  , "playing"
  , "progress"
  , "ratechange"
  , "seeked"
  , "seeking"
  , "stalled"
  , "suspend"
  , "timeupdate"
  , "volumechange"
  , "waiting"
  , "animationend"
  , "animationiteration"
  , "animationstart"
  , "message"
  , "open"
  , "mousewheel"
  , "online"
  , "offline"
  , "popstate"
  , "show"
  , "storage"
  , "toggle"
  , "wheel"
  , "touchcancel"
  , "touchend"
  , "touchmove"
  , "touchstart" ])
