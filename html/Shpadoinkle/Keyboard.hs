{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}


-- | Keyboard key code pattern synonyms to make your keyEvents easier to use.
-- When you use a listener from 'Shpadoinkle.Html.Events' that reads from the
-- Keyboard, you will get the key in question as a 'KeyCode' which just wraps
-- the native 'Int' key code from JavaScript.
--
-- The named aliases are to enhance code readability and ergonomics.
--
-- @
--   Enter = KeyCode 13
-- @
--
-- I saved you from Googling for the key code cowboy, you're welcome.


module Shpadoinkle.Keyboard where


-- | Wrapped raw key code from JavaScript @event.keyCode@
newtype KeyCode = KeyCode { unKeyCode :: Int }
  deriving (Eq, Show, Ord, Enum, Num, Real, Integral)


pattern Backspace    = KeyCode 8
pattern Tab          = KeyCode 9
pattern Enter        = KeyCode 13
pattern Shift        = KeyCode 16
pattern Ctrl         = KeyCode 17
pattern Alt          = KeyCode 18
pattern Pause        = KeyCode 19
pattern Break        = KeyCode 19
pattern CapsLock     = KeyCode 20
pattern Escape       = KeyCode 27
pattern PageUp       = KeyCode 33
pattern PageDown     = KeyCode 34
pattern End          = KeyCode 35
pattern Home         = KeyCode 36
pattern LeftArrow    = KeyCode 37
pattern UpArrow      = KeyCode 38
pattern RightArrow   = KeyCode 39
pattern DownArrow    = KeyCode 40
pattern Insert       = KeyCode 45
pattern Delete       = KeyCode 46
pattern N0           = KeyCode 48
pattern N1           = KeyCode 49
pattern N2           = KeyCode 50
pattern N3           = KeyCode 51
pattern N4           = KeyCode 52
pattern N5           = KeyCode 53
pattern N6           = KeyCode 54
pattern N7           = KeyCode 55
pattern N8           = KeyCode 56
pattern N9           = KeyCode 57
pattern A            = KeyCode 65
pattern B            = KeyCode 66
pattern C            = KeyCode 67
pattern D            = KeyCode 68
pattern E            = KeyCode 69
pattern F            = KeyCode 70
pattern G            = KeyCode 71
pattern H            = KeyCode 72
pattern I            = KeyCode 73
pattern J            = KeyCode 74
pattern K            = KeyCode 75
pattern L            = KeyCode 76
pattern M            = KeyCode 77
pattern N            = KeyCode 78
pattern O            = KeyCode 79
pattern P            = KeyCode 80
pattern Q            = KeyCode 81
pattern R            = KeyCode 82
pattern S            = KeyCode 83
pattern T            = KeyCode 84
pattern U            = KeyCode 85
pattern V            = KeyCode 86
pattern W            = KeyCode 87
pattern X            = KeyCode 88
pattern Y            = KeyCode 89
pattern Z            = KeyCode 90
pattern LeftSuper    = KeyCode 91
pattern RightSuper   = KeyCode 92
pattern Select       = KeyCode 93
pattern Numpad0      = KeyCode 96
pattern Numpad1      = KeyCode 97
pattern Numpad2      = KeyCode 98
pattern Numpad3      = KeyCode 99
pattern Numpad4      = KeyCode 100
pattern Numpad5      = KeyCode 101
pattern Numpad6      = KeyCode 102
pattern Numpad7      = KeyCode 103
pattern Numpad8      = KeyCode 104
pattern Numpad9      = KeyCode 105
pattern Multiply     = KeyCode 106
pattern Add          = KeyCode 107
pattern Subtract     = KeyCode 109
pattern DecimalPoint = KeyCode 110
pattern Divide       = KeyCode 111
pattern F1           = KeyCode 112
pattern F2           = KeyCode 113
pattern F3           = KeyCode 114
pattern F4           = KeyCode 115
pattern F5           = KeyCode 116
pattern F6           = KeyCode 117
pattern F7           = KeyCode 118
pattern F8           = KeyCode 119
pattern F9           = KeyCode 120
pattern F10          = KeyCode 121
pattern F11          = KeyCode 122
pattern F12          = KeyCode 123
pattern NumLock      = KeyCode 144
pattern ScrollLock   = KeyCode 145
pattern SemiColon    = KeyCode 186
pattern EqualSign    = KeyCode 187
pattern Comma        = KeyCode 188
pattern Dash         = KeyCode 189
pattern Period       = KeyCode 190
pattern ForwardSlash = KeyCode 191
pattern GraveAccent  = KeyCode 192
pattern OpenBracket  = KeyCode 219
pattern BackSlash    = KeyCode 220
pattern CloseBraket  = KeyCode 221
pattern SingleQuote  = KeyCode 222
