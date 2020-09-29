-- | This module re-exports the complete HTML DSL,

module Shpadoinkle.Html
  ( module Shpadoinkle.Html.Element
  , module Shpadoinkle.Html.Property
  , module Shpadoinkle.Html.Event
  , module Shpadoinkle.Html.Event.Debounce
  , module Shpadoinkle.Html.Event.Throttle
  , module Shpadoinkle.Html.Utils
  , module Shpadoinkle
  ) where


import           Shpadoinkle                     (Html, Prop, baked, children,
                                                  flagProp, injectProps, listen,
                                                  listenC, listenRaw, listener,
                                                  listenerProp, mapChildren,
                                                  mapProps, name, props, text,
                                                  textContent, textProp)
import           Shpadoinkle.Html.Element
import           Shpadoinkle.Html.Event
import           Shpadoinkle.Html.Event.Debounce
import           Shpadoinkle.Html.Event.Throttle
import           Shpadoinkle.Html.Property
import           Shpadoinkle.Html.Utils
