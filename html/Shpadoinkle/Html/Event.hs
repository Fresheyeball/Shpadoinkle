{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}


-- | This module provides a DSL of Events found on HTML elements.
-- This DSL is entirely optional. You may use the 'Prop's 'PListener' constructor
-- provided by Shpadoinkle core and completely ignore this module.
-- You can use the 'listener', 'listen', 'listenRaw', 'listen'' convenience
-- functions as well, without using this module. But for those who like a typed
-- DSL with named function, and overloading, this is for you.
--
-- All listners come in 2 flavors. Unctuous flavors. Plain (IE 'onInput'), and prime (IE 'onInput'').
-- The following should hold
--
-- @
--   onX (pure x) = onX' x
-- @
--
-- A flavor providing access to the 'RawNode' and the 'RawEvent' are not provided
-- here. If you want access to these try the 'listenRaw' constructor. The intent
-- of this DSL is to provide a simple named functions.
--
-- Right now this module features limited specialization. But ideally we specialize
-- all of these listeners. For example the 'onInput' listener takes a function
-- @(Text -> m o)@ where 'Text' is the current value of the input, and 'onKeyup' takes
-- a function of type @(KeyCode -> m o)@ from 'Shpadoinkle.Keyboard'. Mouse move
-- listeners for example, should take a function of @((Float, Float) -> m o)@ but
-- this work is not yet done. See https://gitlab.com/fresheyeball/Shpadoinkle/issues/5


module Shpadoinkle.Html.Event where


import           Control.Monad               (msum)
import           Data.Text
import           Language.Javascript.JSaddle

import           Shpadoinkle
import           Shpadoinkle.Html.TH
import           Shpadoinkle.Keyboard


onInput' :: MonadJSM m => (Text -> m o) -> (Text, Prop m o)
onInput' f = listenRaw "input" $ \(RawNode n) _ ->
  f =<< liftJSM (valToText =<< unsafeGetProp "value" =<< valToObject n)
onInput :: MonadJSM m => (Text -> o) -> (Text, Prop m o)
onInput f = onInput' (pure . f)


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

onCheck' :: MonadJSM m => (Bool -> m o) -> (Text, Prop m o)
onCheck' f = listenRaw "update" $ \(RawNode n) _ ->
  f =<< liftJSM (valToBool =<< unsafeGetProp "checked" =<< valToObject n)
onCheck :: MonadJSM m => (Bool -> o) -> (Text, Prop m o)
onCheck f = onCheck' (pure . f)

onSubmit' :: MonadJSM m => m o -> (Text, Prop m o)
onSubmit' m = listenRaw "submit" $ \_ (RawEvent e) ->
  liftJSM (valToObject e # ("preventDefault" :: String) $ ([] :: [()])) >> m
onSubmit :: MonadJSM m => o -> (Text, Prop m o)
onSubmit = onSubmit' . pure


mkGlobalKey :: Text -> (KeyCode -> JSM ()) -> JSM ()
mkGlobalKey n t = do
  d <- makeObject =<< jsg ("window" :: Text)
  f <- toJSVal . fun $ \_ _ -> \case
    e:_ -> t =<<
      fmap round (valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)
    _ -> return ()
  unsafeSetProp (toJSString $ "on" <> n) f d


globalKeyDown, globalKeyUp, globalKeyPress :: (KeyCode -> JSM ()) -> JSM ()
globalKeyDown = mkGlobalKey "keydown"
globalKeyUp = mkGlobalKey "keyup"
globalKeyPress = mkGlobalKey "keypress"


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
