{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Widgets.Form.Input where


import           Data.Text

import           Shpadoinkle.Html               as Html
import           Shpadoinkle.Widgets.Types.Core
import           Shpadoinkle.Widgets.Types.Form as Form


input :: MonadJSM m => Props m (Input Text) -> Placeholder -> Input Text -> Html m (Input Text)
input attrs (Placeholder ph) inp = Html.input
  ( Html.type' "text"
  : Html.value (Form.value inp)
  : Html.placeholder ph
  : Html.onInput (Input Dirty)
  : attrs ) []
