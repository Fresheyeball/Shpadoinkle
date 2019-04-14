module Shpadoinkle.Html.Event
  ( onclick , oncontextmenu , ondblclick , onmousedown , onmouseenter , onmouseleave , onmousemove , onmouseover , onmouseout , onmouseup , onkeydown , onkeypress , onkeyup , onbeforeunload , onerror , onhashchange , onload , onpageshow , onpagehide , onresize , onscroll , onunload , onblur , onchange , oncheck , onfocus , onfocusin , onfocusout , oninput , oninvalid , onreset , onsearch , onselect , onsubmit , ondrag , ondragend , ondragenter , ondragleave , ondragover , ondragstart , ondrop , oncopy , oncut , onpaste , onafterprint , onbeforeprint , onabort , oncanplay , oncanplaythrough , ondurationchange , onemptied , onended , onloadeddata , onloadedmetadata , onloadstart , onpause , onplay , onplaying , onprogress , onratechange , onseeked , onseeking , onstalled , onsuspend , ontimeupdate , onvolumechange , onwaiting , animationend , animationiteration , animationstart , onmessage , onopen , onmousewheel , ononline , onoffline , onpopstate , onshow , onstorage , ontoggle , onwheel , ontouchcancel , ontouchend , ontouchmove , ontouchstart , onclick' , oncontextmenu' , ondblclick' , onmousedown' , onmouseenter' , onmouseleave' , onmousemove' , onmouseover' , onmouseout' , onmouseup' , onkeydown' , onkeypress' , onkeyup' , onbeforeunload' , onerror' , onhashchange' , onload' , onpageshow' , onpagehide' , onresize' , onscroll' , onunload' , onblur' , onchange' , oncheck' , onfocus' , onfocusin' , onfocusout' , oninput' , oninvalid' , onreset' , onsearch' , onselect' , onsubmit' , ondrag' , ondragend' , ondragenter' , ondragleave' , ondragover' , ondragstart' , ondrop' , oncopy' , oncut' , onpaste' , onafterprint' , onbeforeprint' , onabort' , oncanplay' , oncanplaythrough' , ondurationchange' , onemptied' , onended' , onloadeddata' , onloadedmetadata' , onloadstart' , onpause' , onplay' , onplaying' , onprogress' , onratechange' , onseeked' , onseeking' , onstalled' , onsuspend' , ontimeupdate' , onvolumechange' , onwaiting' , animationend' , animationiteration' , animationstart' , onmessage' , onopen' , onmousewheel' , ononline' , onoffline' , onpopstate' , onshow' , onstorage' , ontoggle' , onwheel' , ontouchcancel', ontouchend', ontouchmove', ontouchstart'
  , Key(..)
  , toKey) where


import           Shpadoinkle.Class


type ListenerFor a = forall m i o. ((i, a) -> m o) -> (Text, Prop m o)


data Key
  = Enter
  | Unknown


toKey :: String -> Key
toKey "Enter" = Enter
toKey _       = Unknown
listen :: Text -> m o -> (Text, Prop m o)
listen k = (k, ) . PListener
listen' :: Applicative m => Text -> o -> (Text, Prop m o)
listen' k f = listen k $ pure f
listenFor :: Text -> ListenerFor a
listenFor = notYet
onclick :: m o -> (Text, Prop m o)
onclick = listen "click"
onclick' :: Applicative m => o -> (Text, Prop m o)
onclick' = listen' "click"
oncontextmenu :: m o -> (Text, Prop m o)
oncontextmenu = listen "contextmenu"
ondblclick :: m o -> (Text, Prop m o)
ondblclick = listen "dblclick"
onmousedown :: m o -> (Text, Prop m o)
onmousedown = listen "mousedown"
onmouseenter :: m o -> (Text, Prop m o)
onmouseenter = listen "mouseenter"
onmouseleave :: m o -> (Text, Prop m o)
onmouseleave = listen "mouseleave"
onmousemove :: m o -> (Text, Prop m o)
onmousemove = listen "mousemove"
onmouseover :: m o -> (Text, Prop m o)
onmouseover = listen "mouseover"
onmouseout :: m o -> (Text, Prop m o)
onmouseout = listen "mouseout"
onmouseup :: m o -> (Text, Prop m o)
onmouseup = listen "mouseup"
onkeydown :: m o -> (Text, Prop m o)
onkeydown = listen "keydown"
onkeypress :: m o -> (Text, Prop m o)
onkeypress = listen "keypress"
onkeyup :: ListenerFor Key
onkeyup = listenFor "keyup"
onbeforeunload :: m o -> (Text, Prop m o)
onbeforeunload = listen "beforeunload"
onerror :: m o -> (Text, Prop m o)
onerror = listen "error"
onhashchange :: m o -> (Text, Prop m o)
onhashchange = listen "hashchange"
onload :: m o -> (Text, Prop m o)
onload = listen "load"
onpageshow :: m o -> (Text, Prop m o)
onpageshow = listen "pageshow"
onpagehide :: m o -> (Text, Prop m o)
onpagehide = listen "pagehide"
onresize :: m o -> (Text, Prop m o)
onresize = listen "resize"
onscroll :: m o -> (Text, Prop m o)
onscroll = listen "scroll"
onunload :: m o -> (Text, Prop m o)
onunload = listen "unload"
onblur :: m o -> (Text, Prop m o)
onblur = listen "blur"
onchange :: m o -> (Text, Prop m o)
onchange = listen "change"
oncheck :: ListenerFor Bool
oncheck = listenFor "change"
onfocus :: m o -> (Text, Prop m o)
onfocus = listen "focus"
onfocusin :: m o -> (Text, Prop m o)
onfocusin = listen "focusin"
onfocusout :: m o -> (Text, Prop m o)
onfocusout = listen "focusout"
oninput :: ListenerFor Text
oninput = listenFor "input"
oninvalid :: m o -> (Text, Prop m o)
oninvalid = listen "invalid"
onreset :: m o -> (Text, Prop m o)
onreset = listen "reset"
onsearch :: m o -> (Text, Prop m o)
onsearch = listen "search"
onselect :: m o -> (Text, Prop m o)
onselect = listen "select"
onsubmit :: m o -> (Text, Prop m o)
onsubmit = listen "submit"
ondrag :: m o -> (Text, Prop m o)
ondrag = listen "drag"
ondragend :: m o -> (Text, Prop m o)
ondragend = listen "dragend"
ondragenter :: m o -> (Text, Prop m o)
ondragenter = listen "dragenter"
ondragleave :: m o -> (Text, Prop m o)
ondragleave = listen "dragleave"
ondragover :: m o -> (Text, Prop m o)
ondragover = listen "dragover"
ondragstart :: m o -> (Text, Prop m o)
ondragstart = listen "dragstart"
ondrop :: m o -> (Text, Prop m o)
ondrop = listen "drop"
oncopy :: m o -> (Text, Prop m o)
oncopy = listen "copy"
oncut :: m o -> (Text, Prop m o)
oncut = listen "cut"
onpaste :: m o -> (Text, Prop m o)
onpaste = listen "paste"
onafterprint :: m o -> (Text, Prop m o)
onafterprint = listen "afterprint"
onbeforeprint :: m o -> (Text, Prop m o)
onbeforeprint = listen "beforeprint"
onabort :: m o -> (Text, Prop m o)
onabort = listen "abort"
oncanplay :: m o -> (Text, Prop m o)
oncanplay = listen "canplay"
oncanplaythrough :: m o -> (Text, Prop m o)
oncanplaythrough = listen "canplaythrough"
ondurationchange :: m o -> (Text, Prop m o)
ondurationchange = listen "durationchange"
onemptied :: m o -> (Text, Prop m o)
onemptied = listen "emptied"
onended :: m o -> (Text, Prop m o)
onended = listen "ended"
onloadeddata :: m o -> (Text, Prop m o)
onloadeddata = listen "loadeddata"
onloadedmetadata :: m o -> (Text, Prop m o)
onloadedmetadata = listen "loadedmetadata"
onloadstart :: m o -> (Text, Prop m o)
onloadstart = listen "loadstart"
onpause :: m o -> (Text, Prop m o)
onpause = listen "pause"
onplay :: m o -> (Text, Prop m o)
onplay = listen "play"
onplaying :: m o -> (Text, Prop m o)
onplaying = listen "playing"
onprogress :: m o -> (Text, Prop m o)
onprogress = listen "progress"
onratechange :: m o -> (Text, Prop m o)
onratechange = listen "ratechange"
onseeked :: m o -> (Text, Prop m o)
onseeked = listen "seeked"
onseeking :: m o -> (Text, Prop m o)
onseeking = listen "seeking"
onstalled :: m o -> (Text, Prop m o)
onstalled = listen "stalled"
onsuspend :: m o -> (Text, Prop m o)
onsuspend = listen "suspend"
ontimeupdate :: m o -> (Text, Prop m o)
ontimeupdate = listen "timeupdate"
onvolumechange :: m o -> (Text, Prop m o)
onvolumechange = listen "volumechange"
onwaiting :: m o -> (Text, Prop m o)
onwaiting = listen "waiting"
animationend :: m o -> (Text, Prop m o)
animationend = listen "animationend"
animationiteration :: m o -> (Text, Prop m o)
animationiteration = listen "animationiteration"
animationstart :: m o -> (Text, Prop m o)
animationstart = listen "animationstart"
onmessage :: m o -> (Text, Prop m o)
onmessage = listen "message"
onopen :: m o -> (Text, Prop m o)
onopen = listen "open"
onmousewheel :: m o -> (Text, Prop m o)
onmousewheel = listen "mousewheel"
ononline :: m o -> (Text, Prop m o)
ononline = listen "online"
onoffline :: m o -> (Text, Prop m o)
onoffline = listen "offline"
onpopstate :: m o -> (Text, Prop m o)
onpopstate = listen "popstate"
onshow :: m o -> (Text, Prop m o)
onshow = listen "show"
onstorage :: m o -> (Text, Prop m o)
onstorage = listen "storage"
ontoggle :: m o -> (Text, Prop m o)
ontoggle = listen "toggle"
onwheel :: m o -> (Text, Prop m o)
onwheel = listen "wheel"
ontouchcancel :: m o -> (Text, Prop m o)
ontouchcancel = listen "touchcancel"
ontouchend :: m o -> (Text, Prop m o)
ontouchend = listen "touchend"
ontouchmove :: m o -> (Text, Prop m o)
ontouchmove = listen "touchmove"
ontouchstart :: m o -> (Text, Prop m o)
ontouchstart = listen "touchstart"
oncontextmenu' :: Applicative m => o -> (Text, Prop m o)
oncontextmenu' = listen' "contextmenu"
ondblclick' :: Applicative m => o -> (Text, Prop m o)
ondblclick' = listen' "dblclick"
onmousedown' :: Applicative m => o -> (Text, Prop m o)
onmousedown' = listen' "mousedown"
onmouseenter' :: Applicative m => o -> (Text, Prop m o)
onmouseenter' = listen' "mouseenter"
onmouseleave' :: Applicative m => o -> (Text, Prop m o)
onmouseleave' = listen' "mouseleave"
onmousemove' :: Applicative m => o -> (Text, Prop m o)
onmousemove' = listen' "mousemove"
onmouseover' :: Applicative m => o -> (Text, Prop m o)
onmouseover' = listen' "mouseover"
onmouseout' :: Applicative m => o -> (Text, Prop m o)
onmouseout' = listen' "mouseout"
onmouseup' :: Applicative m => o -> (Text, Prop m o)
onmouseup' = listen' "mouseup"
onkeydown' :: Applicative m => o -> (Text, Prop m o)
onkeydown' = listen' "keydown"
onkeypress' :: Applicative m => o -> (Text, Prop m o)
onkeypress' = listen' "keypress"
onkeyup' :: ListenerFor Key
onkeyup' = listenFor "keyup"
onbeforeunload' :: Applicative m => o -> (Text, Prop m o)
onbeforeunload' = listen' "beforeunload"
onerror' :: Applicative m => o -> (Text, Prop m o)
onerror' = listen' "error"
onhashchange' :: Applicative m => o -> (Text, Prop m o)
onhashchange' = listen' "hashchange"
onload' :: Applicative m => o -> (Text, Prop m o)
onload' = listen' "load"
onpageshow' :: Applicative m => o -> (Text, Prop m o)
onpageshow' = listen' "pageshow"
onpagehide' :: Applicative m => o -> (Text, Prop m o)
onpagehide' = listen' "pagehide"
onresize' :: Applicative m => o -> (Text, Prop m o)
onresize' = listen' "resize"
onscroll' :: Applicative m => o -> (Text, Prop m o)
onscroll' = listen' "scroll"
onunload' :: Applicative m => o -> (Text, Prop m o)
onunload' = listen' "unload"
onblur' :: Applicative m => o -> (Text, Prop m o)
onblur' = listen' "blur"
onchange' :: Applicative m => o -> (Text, Prop m o)
onchange' = listen' "change"
oncheck' :: ListenerFor Bool
oncheck' = listenFor "change"
onfocus' :: Applicative m => o -> (Text, Prop m o)
onfocus' = listen' "focus"
onfocusin' :: Applicative m => o -> (Text, Prop m o)
onfocusin' = listen' "focusin"
onfocusout' :: Applicative m => o -> (Text, Prop m o)
onfocusout' = listen' "focusout"
oninput' :: ListenerFor String
oninput' = listenFor "input"
oninvalid' :: Applicative m => o -> (Text, Prop m o)
oninvalid' = listen' "invalid"
onreset' :: Applicative m => o -> (Text, Prop m o)
onreset' = listen' "reset"
onsearch' :: Applicative m => o -> (Text, Prop m o)
onsearch' = listen' "search"
onselect' :: Applicative m => o -> (Text, Prop m o)
onselect' = listen' "select"
onsubmit' :: Applicative m => o -> (Text, Prop m o)
onsubmit' = listen' "submit"
ondrag' :: Applicative m => o -> (Text, Prop m o)
ondrag' = listen' "drag"
ondragend' :: Applicative m => o -> (Text, Prop m o)
ondragend' = listen' "dragend"
ondragenter' :: Applicative m => o -> (Text, Prop m o)
ondragenter' = listen' "dragenter"
ondragleave' :: Applicative m => o -> (Text, Prop m o)
ondragleave' = listen' "dragleave"
ondragover' :: Applicative m => o -> (Text, Prop m o)
ondragover' = listen' "dragover"
ondragstart' :: Applicative m => o -> (Text, Prop m o)
ondragstart' = listen' "dragstart"
ondrop' :: Applicative m => o -> (Text, Prop m o)
ondrop' = listen' "drop"
oncopy' :: Applicative m => o -> (Text, Prop m o)
oncopy' = listen' "copy"
oncut' :: Applicative m => o -> (Text, Prop m o)
oncut' = listen' "cut"
onpaste' :: Applicative m => o -> (Text, Prop m o)
onpaste' = listen' "paste"
onafterprint' :: Applicative m => o -> (Text, Prop m o)
onafterprint' = listen' "afterprint"
onbeforeprint' :: Applicative m => o -> (Text, Prop m o)
onbeforeprint' = listen' "beforeprint"
onabort' :: Applicative m => o -> (Text, Prop m o)
onabort' = listen' "abort"
oncanplay' :: Applicative m => o -> (Text, Prop m o)
oncanplay' = listen' "canplay"
oncanplaythrough' :: Applicative m => o -> (Text, Prop m o)
oncanplaythrough' = listen' "canplaythrough"
ondurationchange' :: Applicative m => o -> (Text, Prop m o)
ondurationchange' = listen' "durationchange"
onemptied' :: Applicative m => o -> (Text, Prop m o)
onemptied' = listen' "emptied"
onended' :: Applicative m => o -> (Text, Prop m o)
onended' = listen' "ended"
onloadeddata' :: Applicative m => o -> (Text, Prop m o)
onloadeddata' = listen' "loadeddata"
onloadedmetadata' :: Applicative m => o -> (Text, Prop m o)
onloadedmetadata' = listen' "loadedmetadata"
onloadstart' :: Applicative m => o -> (Text, Prop m o)
onloadstart' = listen' "loadstart"
onpause' :: Applicative m => o -> (Text, Prop m o)
onpause' = listen' "pause"
onplay' :: Applicative m => o -> (Text, Prop m o)
onplay' = listen' "play"
onplaying' :: Applicative m => o -> (Text, Prop m o)
onplaying' = listen' "playing"
onprogress' :: Applicative m => o -> (Text, Prop m o)
onprogress' = listen' "progress"
onratechange' :: Applicative m => o -> (Text, Prop m o)
onratechange' = listen' "ratechange"
onseeked' :: Applicative m => o -> (Text, Prop m o)
onseeked' = listen' "seeked"
onseeking' :: Applicative m => o -> (Text, Prop m o)
onseeking' = listen' "seeking"
onstalled' :: Applicative m => o -> (Text, Prop m o)
onstalled' = listen' "stalled"
onsuspend' :: Applicative m => o -> (Text, Prop m o)
onsuspend' = listen' "suspend"
ontimeupdate' :: Applicative m => o -> (Text, Prop m o)
ontimeupdate' = listen' "timeupdate"
onvolumechange' :: Applicative m => o -> (Text, Prop m o)
onvolumechange' = listen' "volumechange"
onwaiting' :: Applicative m => o -> (Text, Prop m o)
onwaiting' = listen' "waiting"
animationend' :: Applicative m => o -> (Text, Prop m o)
animationend' = listen' "animationend"
animationiteration' :: Applicative m => o -> (Text, Prop m o)
animationiteration' = listen' "animationiteration"
animationstart' :: Applicative m => o -> (Text, Prop m o)
animationstart' = listen' "animationstart"
onmessage' :: Applicative m => o -> (Text, Prop m o)
onmessage' = listen' "message"
onopen' :: Applicative m => o -> (Text, Prop m o)
onopen' = listen' "open"
onmousewheel' :: Applicative m => o -> (Text, Prop m o)
onmousewheel' = listen' "mousewheel"
ononline' :: Applicative m => o -> (Text, Prop m o)
ononline' = listen' "online"
onoffline' :: Applicative m => o -> (Text, Prop m o)
onoffline' = listen' "offline"
onpopstate' :: Applicative m => o -> (Text, Prop m o)
onpopstate' = listen' "popstate"
onshow' :: Applicative m => o -> (Text, Prop m o)
onshow' = listen' "show"
onstorage' :: Applicative m => o -> (Text, Prop m o)
onstorage' = listen' "storage"
ontoggle' :: Applicative m => o -> (Text, Prop m o)
ontoggle' = listen' "toggle"
onwheel' :: Applicative m => o -> (Text, Prop m o)
onwheel' = listen' "wheel"
ontouchcancel' :: Applicative m => o -> (Text, Prop m o)
ontouchcancel' = listen' "touchcancel"
ontouchend' :: Applicative m => o -> (Text, Prop m o)
ontouchend' = listen' "touchend"
ontouchmove' :: Applicative m => o -> (Text, Prop m o)
ontouchmove' = listen' "touchmove"
ontouchstart' :: Applicative m => o -> (Text, Prop m o)
ontouchstart' = listen' "touchstart"
