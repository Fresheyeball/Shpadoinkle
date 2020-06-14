{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Shpadoinkle.Widgets.Form.Input where


import           Data.Coerce
import           Data.Text
import           Data.Text.Read

import           Shpadoinkle                      hiding (text)
import           Shpadoinkle.Html                 as Html
import           Shpadoinkle.Widgets.Types.Core
import           Shpadoinkle.Widgets.Types.Form   as Form
import           Shpadoinkle.Widgets.Types.Search


type Config a = Props' (Input a)


mkInput :: Text -> (Text -> a) -> (a -> Text) -> Config a -> Input a -> Html' (Input a)
mkInput t to from attrs inp = Html.input
  ( Html.value (from $ Form._value inp)
  : Html.onInput' (Input Dirty . to)
  : Html.type' t
  : attrs ) []


fractional :: Fractional n => Show n => Config n -> Input n -> Html' (Input n)
fractional cfg inp = mkInput "number" to (pack . show) cfg inp where
  to t = case double t of
    Right (d,"") -> realToFrac d
    _            -> _value inp


integral :: Integral n => Show n => Config n -> Input n -> Html' (Input n)
integral cfg inp = mkInput "number" to (pack . show) cfg inp where
  to t = case double t of
    Right (d,"") -> round d
    _            -> _value inp


search :: Config Search -> Input Search -> Html' (Input Search)
search = mkInput "search" coerce coerce


text :: forall t. Coercible Text t => Config t -> Input t -> Html' (Input t)
text = mkInput "text" coerce coerce
