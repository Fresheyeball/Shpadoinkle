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
