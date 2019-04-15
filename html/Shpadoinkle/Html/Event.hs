{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}


module Shpadoinkle.Html.Event where


import           Control.Monad       (msum)
import           Data.Text

import           Shpadoinkle
import           Shpadoinkle.Html.TH


listen :: Text -> m o -> (Text, Prop m o)
listen k = (,) k . PListener
listen' :: Applicative m => Text -> o -> (Text, Prop m o)
listen' k f = listen k $ pure f


$(fmap msum $ mapM mkEventDSL
  [ "click"
  , "contextmenu"
  , "dblclick"
  , "mousedown"
  , "mouseleave"
  , "mousemove"
  , "mouseover"
  , "mouseout"
  , "mouseup"
  , "keydown"
  , "keypress"
  , "keyup"
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
  , "input"
  , "invalid"
  , "reset"
  , "search"
  , "select"
  , "submit"
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
