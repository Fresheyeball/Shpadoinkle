{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}


-- | This module provides a DSL of Events found on HTML elements.
-- This DSL is entirely optional. You may use the 'Prop's 'PListener' constructor
-- provided by Shpadoinkle core and completely ignore this module.
-- You can use the 'listener', 'listen, 'listenRaw', 'listenE', 'listenM' convenience
-- functions as well, without using this module. But for those who like a typed
-- DSL with named function, and overloading, this is for you.
--
-- All listners come in 2 flavors. Unctuous flavors. Plain (IE 'onInput'), and monadic (IE 'onInputM').
-- The following should hold
--
-- @
--   onXM (pure x) = onX x
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


import           Control.Monad               (msum, void)
import           Data.Proxy
import           Data.Text
import           Language.Javascript.JSaddle

import           Shpadoinkle
import           Shpadoinkle.Html.TH
import           Shpadoinkle.Keyboard


mkWithFormVal :: IsProp p e => (JSVal -> JSM v) -> Text -> JSString -> (v -> e a) -> (Text, p a)
mkWithFormVal valTo evt from f = listenRaw evt $ \(RawNode n) _ ->
  return . f =<< liftJSM (valTo =<< unsafeGetProp from =<< valToObject n)


onInputE :: IsProp p e => (Text -> e a) -> (Text, p a)
onInputE = mkWithFormVal valToText "input" "value"


onInput :: forall p e a. IsProp p e => (Text -> a) -> (Text, p a)
onInput f = onInputE (constUpdate (Proxy :: Proxy p) . f)


onInputM :: Monad m => (Text -> m (a -> a)) -> (Text, PropM m a)
onInputM f = onInputE (impur . f)


onInputM_ :: Monad m => (Text -> m ()) -> (Text, PropM m a)
onInputM_ f = onInputE (causes . f)


onOptionE :: IsProp p e => (Text -> e a) -> (Text, p a)
onOptionE = mkWithFormVal valToText "change" "value"


onOption :: forall p e a. IsProp p e => (Text -> a) -> (Text, p a)
onOption f = onOptionE (constUpdate (Proxy :: Proxy p) . f)


onOptionM :: Monad m => (Text -> m (a -> a)) -> (Text, PropM m a)
onOptionM f = onOptionE (impur . f)


onOptionM_ :: Monad m => (Text -> m ()) -> (Text, PropM m a)
onOptionM_ f = onOptionE (causes . f)


mkOnKey :: IsProp p e => Text -> (KeyCode -> e a) -> (Text, p a)
mkOnKey t f = listenRaw t $ \_ (RawEvent e) ->
  return . f =<< liftJSM (fmap round $ valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)


onKeyupE, onKeydownE, onKeypressE :: IsProp p e => (KeyCode -> e a) -> (Text, p a)
onKeyupE    = mkOnKey "keyup"
onKeydownE  = mkOnKey "keydown"
onKeypressE = mkOnKey "keypress"
onKeyup, onKeydown, onKeypress  :: forall p e a. IsProp p e => (KeyCode -> a) -> (Text, p a)
onKeyup    f = onKeyupE    (constUpdate (Proxy :: Proxy p) . f)
onKeydown  f = onKeydownE  (constUpdate (Proxy :: Proxy p) . f)
onKeypress f = onKeypressE (constUpdate (Proxy :: Proxy p) . f)
onKeyupM, onKeydownM, onKeypressM :: Monad m => (KeyCode -> m (a -> a)) -> (Text, PropM m a)
onKeyupM    f = onKeyupE    (impur . f)
onKeydownM  f = onKeydownE  (impur . f)
onKeypressM f = onKeypressE (impur . f)
onKeyupM_, onKeydownM_, onKeypressM_ :: Monad m => (KeyCode -> m ()) -> (Text, PropM m a)
onKeyupM_    f = onKeyupE    (causes . f)
onKeydownM_  f = onKeydownE  (causes . f)
onKeypressM_ f = onKeypressE (causes . f)


onCheckE :: IsProp p e => (Bool -> e a) -> (Text, p a)
onCheckE = mkWithFormVal valToBool "change" "checked"


onCheck :: forall p e a. IsProp p e => (Bool -> a) -> (Text, p a)
onCheck f = onCheckE (constUpdate (Proxy :: Proxy p) . f)


onCheckM :: Monad m => (Bool -> m (a -> a)) -> (Text, PropM m a)
onCheckM f = onCheckE (impur . f)


onCheckM_ :: Monad m => (Bool -> m ()) -> (Text, PropM m a)
onCheckM_ f = onCheckE (causes . f)


preventDefault :: RawEvent -> JSM ()
preventDefault e = void $ valToObject e # ("preventDefault" :: String) $ ([] :: [()])


onSubmitE :: IsProp p e => e a -> (Text, p a)
onSubmitE m = listenRaw "submit" $ \_ e -> preventDefault e >> return m


onSubmit :: forall p e a. IsProp p e => a -> (Text, p a)
onSubmit = onSubmitE . constUpdate (Proxy :: Proxy p)


onSubmitM :: Monad m => m (a -> a) -> (Text, PropM m a)
onSubmitM = onSubmitE . impur


onSubmitM_ :: Monad m => m () -> (Text, PropM m a)
onSubmitM_ = onSubmitE . causes


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
