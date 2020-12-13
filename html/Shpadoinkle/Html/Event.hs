{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}


-- | This module provides a DSL of Events found on HTML elements.
-- This DSL is entirely optional. You may use the 'Prop's 'PListener' constructor
-- provided by Shpadoinkle core and completely ignore this module.
-- You can use the 'listener', 'listen', 'listenRaw', 'listenC', and 'listenM' convenience
-- functions as well without using this module. For those who like a typed
-- DSL with named functions and overloading, this is for you.
--
-- All listeners come in 2 flavors. Unctuous flavors. Plain (i.e. 'onInput') and monadic (i.e. 'onInputM').
-- The following should hold
--
-- @
--   onXM (pure x) = onX x
-- @
--
-- A flavor providing access to the 'RawNode' and the 'RawEvent' are not provided
-- here. If you want access to these, try the 'listenRaw' constructor. The intent
-- of this DSL is to provide simple named functions.
--
-- Right now this module features limited specialization, but ideally we specialize
-- all of these listeners. For example, the 'onInput' listener takes a function
-- @(Text -> m a)@ where 'Text' is the current value of the input and 'onKeyup' takes
-- a function of type @(KeyCode -> m a)@ from 'Shpadoinkle.Keyboard'. Mouse move
-- listeners, for example, should take a function of @((Float, Float) -> m a)@, but
-- this work is not yet done. See https://gitlab.com/fresheyeball/Shpadoinkle/issues/5


module Shpadoinkle.Html.Event where


import           Control.Concurrent.STM      (retry)
import           Control.Lens                ((^.))
import           Control.Monad               (msum, void)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Text
import           GHCJS.DOM.Types             hiding (Text)
import           Language.Javascript.JSaddle hiding (JSM, liftJSM, toJSString)
import           UnliftIO.Concurrent         (forkIO)
import           UnliftIO.STM

import           Shpadoinkle
import           Shpadoinkle.Html.TH
import           Shpadoinkle.Keyboard


mkWithFormVal ::  (JSVal -> JSM v) -> Text -> JSString -> (v -> Continuation m a) -> (Text, Prop m a)
mkWithFormVal valTo evt from f = listenRaw evt $ \(RawNode n) _ ->
  f <$> liftJSM (valTo =<< unsafeGetProp from =<< valToObject n)


onInputC ::  (Text -> Continuation m a) -> (Text, Prop m a)
onInputC = mkWithFormVal valToText "input" "value"


onInput :: (Text -> a) -> (Text, Prop m a)
onInput f = onInputC (constUpdate . f)


onInputM :: Monad m => (Text -> m (a -> a)) -> (Text, Prop m a)
onInputM f = onInputC (impur . f)


onInputM_ :: Monad m => (Text -> m ()) -> (Text, Prop m a)
onInputM_ f = onInputC (causes . f)


onOptionC ::  (Text -> Continuation m a) -> (Text, Prop m a)
onOptionC = mkWithFormVal valToText "change" "value"


onOption :: (Text -> a) -> (Text, Prop m a)
onOption f = onOptionC (constUpdate . f)


onOptionM :: Monad m => (Text -> m (a -> a)) -> (Text, Prop m a)
onOptionM f = onOptionC (impur . f)


onOptionM_ :: Monad m => (Text -> m ()) -> (Text, Prop m a)
onOptionM_ f = onOptionC (causes . f)


mkOnKey ::  Text -> (KeyCode -> Continuation m a) -> (Text, Prop m a)
mkOnKey t f = listenRaw t $ \_ (RawEvent e) ->
  f <$> liftJSM (fmap round $ valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)


onKeyupC, onKeydownC, onKeypressC :: (KeyCode -> Continuation m a) -> (Text, Prop m a)
onKeyupC    = mkOnKey "keyup"
onKeydownC  = mkOnKey "keydown"
onKeypressC = mkOnKey "keypress"
onKeyup, onKeydown, onKeypress  :: (KeyCode -> a) -> (Text, Prop m a)
onKeyup    f = onKeyupC    (constUpdate . f)
onKeydown  f = onKeydownC  (constUpdate . f)
onKeypress f = onKeypressC (constUpdate . f)
onKeyupM, onKeydownM, onKeypressM :: Monad m => (KeyCode -> m (a -> a)) -> (Text, Prop m a)
onKeyupM    f = onKeyupC    (impur . f)
onKeydownM  f = onKeydownC  (impur . f)
onKeypressM f = onKeypressC (impur . f)
onKeyupM_, onKeydownM_, onKeypressM_ :: Monad m => (KeyCode -> m ()) -> (Text, Prop m a)
onKeyupM_    f = onKeyupC    (causes . f)
onKeydownM_  f = onKeydownC  (causes . f)
onKeypressM_ f = onKeypressC (causes . f)


onCheckC ::  (Bool -> Continuation m a) -> (Text, Prop m a)
onCheckC = mkWithFormVal valToBool "change" "checked"


onCheck :: (Bool -> a) -> (Text, Prop m a)
onCheck f = onCheckC (constUpdate . f)


onCheckM :: Monad m => (Bool -> m (a -> a)) -> (Text, Prop m a)
onCheckM f = onCheckC (impur . f)


onCheckM_ :: Monad m => (Bool -> m ()) -> (Text, Prop m a)
onCheckM_ f = onCheckC (causes . f)


preventDefault :: RawEvent -> JSM ()
preventDefault e = void $ valToObject e # ("preventDefault" :: String) $ ([] :: [()])


onSubmitC :: Continuation m a -> (Text, Prop m a)
onSubmitC m = listenRaw "submit" $ \_ e -> preventDefault e >> return m


onSubmit :: a -> (Text, Prop m a)
onSubmit = onSubmitC . constUpdate


onSubmitM :: Monad m => m (a -> a) -> (Text, Prop m a)
onSubmitM = onSubmitC . impur


onSubmitM_ :: Monad m => m () -> (Text, Prop m a)
onSubmitM_ = onSubmitC . causes


onClickAwayC :: Continuation m a -> (Text, Prop m a)
onClickAwayC c =
  ( "onclickaway"
  , PPotato $ \(RawNode elm) -> liftJSM $ do

     (notify, twas) <- liftIO $ (,) <$> newTVarIO 0 <*> newTVarIO (0 :: Int)

     void $ jsg ("document" :: Text) ^. js2 ("addEventListener" :: Text) ("click" :: Text)
        (fun $ \_ _ -> \case
          evt:_ -> void . forkIO $ do

            target   <- evt ^. js ("target" :: Text)
            onTarget <- fromJSVal =<< elm ^. js1 ("contains" :: Text) target
            case onTarget of
              Just False -> atomically $ modifyTVar notify (+ 1)
              _          -> return ()

          [] -> pure ())

     return $ do
       new' <- readTVar notify
       old  <- readTVar twas
       if new' == old then retry else c <$ writeTVar twas new'
  )


onClickAway :: a -> (Text, Prop m a)
onClickAway = onClickAwayC . constUpdate


onClickAwayM :: Monad m => m (a -> a) -> (Text, Prop m a)
onClickAwayM = onClickAwayC . impur


onClickAwayM_ :: Monad m => m () -> (Text, Prop m a)
onClickAwayM_ = onClickAwayC . causes


mkGlobalKey :: Text -> (KeyCode -> JSM ()) -> JSM ()
mkGlobalKey n t = do
  d <- makeObject =<< jsg ("window" :: Text)
  f <- toJSVal . fun $ \_ _ -> \case
    e:_ -> (t . round) =<<
      (valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)
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
  , "mouseenter"
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
