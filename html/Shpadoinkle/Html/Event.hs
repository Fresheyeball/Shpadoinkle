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
--   onX' (pure x) = onX x
-- @
--
-- A flavor providing access to the 'RawNode' and the 'RawEvent' are not provided
-- here. If you want access to these try the 'listenRaw' constructor. The intent
-- of this DSL is to provide a simple named functions.
--
-- Right now this module features limited specialization. But ideally we specialize
-- all of these listeners. For example the 'onInput' listener takes a function
-- @(Text -> m a)@ where 'Text' is the current value of the input, and 'onKeyup' takes
-- a function of type @(KeyCode -> m a)@ from 'Shpadoinkle.Keyboard'. Mouse move
-- listeners for example, should take a function of @((Float, Float) -> m a)@ but
-- this work is not yet done. See https://gitlab.com/fresheyeball/Shpadoinkle/issues/5


module Shpadoinkle.Html.Event where


import           Control.Monad               (msum)
import           Data.Text
import           Language.Javascript.JSaddle

import           Shpadoinkle
import           Shpadoinkle.Html.TH
import           Shpadoinkle.Keyboard


mkWithFormVal :: MonadJSM m => (JSVal -> JSM v) -> Text -> JSString -> (v -> m (Continuation m a)) -> (Text, Prop m a)
mkWithFormVal valTo evt from f = listenRaw evt $ \(RawNode n) _ ->
  f =<< liftJSM (valTo =<< unsafeGetProp from =<< valToObject n)


onInput' :: MonadJSM m => (Text -> m (Continuation m a)) -> (Text, Prop m a)
onInput' = mkWithFormVal valToText "input" "value"


onInput :: MonadJSM m => (Text -> Continuation m a) -> (Text, Prop m a)
onInput f = onInput' (pure . f)


onOption' :: MonadJSM m => (Text -> m (Continuation m a)) -> (Text, Prop m a)
onOption' = mkWithFormVal valToText "change" "value"


onOption :: MonadJSM m => (Text -> Continuation m a) -> (Text, Prop m a)
onOption f = onOption' (pure . f)


mkOnKey :: MonadJSM m => Text -> (KeyCode -> m (Continuation m a)) -> (Text, Prop m a)
mkOnKey t f = listenRaw t $ \_ (RawEvent e) ->
  f =<< liftJSM (fmap round $ valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)


onKeyup, onKeydown, onKeypress :: MonadJSM m => (KeyCode -> m (Continuation m a)) -> (Text, Prop m a)
onKeyup    = mkOnKey "keyup"
onKeydown  = mkOnKey "keydown"
onKeypress = mkOnKey "keypress"
onKeyup', onKeydown', onKeypress' :: MonadJSM m => (KeyCode -> Continuation m a) -> (Text, Prop m a)
onKeyup'    f = onKeyup    (pure . f)
onKeydown'  f = onKeydown  (pure . f)
onKeypress' f = onKeypress (pure . f)


onCheck' :: MonadJSM m => (Bool -> m (Continuation m a)) -> (Text, Prop m a)
onCheck' = mkWithFormVal valToBool "change" "checked"


onCheck :: MonadJSM m => (Bool -> Continuation m a) -> (Text, Prop m a)
onCheck f = onCheck' (pure . f)


onSubmit' :: MonadJSM m => m (Continuation m a) -> (Text, Prop m a)
onSubmit' m = listenRaw "submit" $ \_ (RawEvent e) ->
  liftJSM (valToObject e # ("preventDefault" :: String) $ ([] :: [()])) >> m


onSubmit :: MonadJSM m => Continuation m a -> (Text, Prop m a)
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
globalKeyDown  = mkGlobalKey "keydown"
globalKeyUp    = mkGlobalKey "keyup"
globalKeyPress = mkGlobalKey "keypress"


$(msum <$> mapM mkEventDSL
  [ "click"
  , "change"
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
