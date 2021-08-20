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
-- All listeners come in 4 flavors. Unctuous flavors. Plain ('onInput'), continuous ('onInputC'), monadic ('onInputM'), and forgetful ('onInputM_').
--
-- A flavor providing access to the 'RawNode' and the 'RawEvent' are not provided
-- here. If you want access to these, try the 'listenRaw' constructor. The intent
-- of this DSL is to provide simple named functions.
--
-- Right now this module features limited specialization, but ideally we specialize
-- all of these listeners. For example, the 'onInput' listener takes a function
-- @(Text -> a -> a)@ where 'Text' is the current value of the input and 'onKeyup' takes
-- a function of type @(KeyCode -> a -> a)@ from 'Shpadoinkle.Keyboard'. Mouse move
-- listeners, for example, should take a function of @((Float, Float) -> a -> a)@, but
-- this work is not yet done. See https://gitlab.com/platonic/shpadoinkle/issues/5


module Shpadoinkle.Html.Event.Basic where


import           Control.Monad       (msum)

import           Shpadoinkle
import           Shpadoinkle.Html.TH


$(msum <$> mapM mkEventDSL
  [ "abort"
  , "afterprint"
  , "animationcancel"
  , "animationend"
  , "animationiteration"
  , "animationstart"
--  , "beforeinput" - onBeforeinput is provided in Shpadoinkle.Html.Event
  , "beforeprint"
  , "beforeunload"
  , "blur"
  , "canplay"
  , "canplaythrough"
  , "change"
  , "click"
  , "contextmenu"
  , "copy"
  , "cut"
  , "dblclick"
  , "drag"
  , "dragend"
  , "dragenter"
  , "dragleave"
  , "dragover"
  , "dragstart"
  , "drop"
  , "durationchange"
  , "emptied"
  , "ended"
  , "error"
  , "focus"
  , "focusin"
  , "focusout"
  , "gotpointercapture"
  , "hashchange"
--  , "input" - onInput is provided in Shpadoinkle.Html.Event
  , "invalid"
  , "load"
  , "loadeddata"
  , "loadedmetadata"
  , "loadstart"
  , "lostpointercapture"
  , "message"
  , "mousedown"
  , "mouseenter"
  , "mouseleave"
  , "mousemove"
  , "mouseout"
  , "mouseover"
  , "mouseup"
  , "mousewheel"
  , "offline"
  , "online"
  , "open"
  , "pagehide"
  , "pageshow"
  , "paste"
  , "pause"
  , "play"
  , "playing"
  , "pointercancel"
  , "pointerdown"
  , "pointerenter"
  , "pointerleave"
  , "pointermove"
  , "pointerout"
  , "pointerover"
  , "pointerup"
  , "popstate"
  , "progress"
  , "ratechange"
  , "reset"
  , "resize"
  , "scroll"
  , "search"
  , "seeked"
  , "seeking"
  , "select"
  , "show"
  , "stalled"
  , "storage"
  , "suspend"
  , "timeupdate"
  , "toggle"
  , "touchcancel"
  , "touchend"
  , "touchmove"
  , "touchstart"
  , "transitioncancel"
  , "transitionend"
  , "transitionrun"
  , "transitionstart"
  , "unload"
  , "volumechange"
  , "waiting"
  , "wheel" ])
