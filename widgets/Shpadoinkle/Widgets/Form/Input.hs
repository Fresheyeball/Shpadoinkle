{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Shpadoinkle.Widgets.Form.Input where


import           Data.Coerce                      (Coercible, coerce)
import           Data.Text                        (Text, pack)
import           Data.Text.Read                   (double)

import           Shpadoinkle                      (Html, Prop)
import           Shpadoinkle.Html                 as Html (input, onInput,
                                                           type', value)
import           Shpadoinkle.Widgets.Types.Core   (Hygiene (Dirty))
import           Shpadoinkle.Widgets.Types.Form   as Form (Input (Input, _value))
import           Shpadoinkle.Widgets.Types.Search (Search (Search))


type Config m a = [(Text, Prop m (Input a))]


mkInput :: Text -> (Text -> a) -> (a -> Text) -> Config m a -> Input a -> Html m (Input a)
mkInput t to from attrs inp = Html.input
  ( Html.value (from $ Form._value inp)
  : Html.onInput (\x _ -> Input Dirty $ to x)
  : Html.type' t
  : attrs ) []


fractional :: Fractional n => Show n => Config m n -> Input n -> Html m (Input n)
fractional cfg inp = mkInput "number" to (pack . show) cfg inp where
  to t = case double t of
    Right (d,"") -> realToFrac d
    _            -> _value inp


integral :: Integral n => Show n => Config m n -> Input n -> Html m (Input n)
integral cfg inp = mkInput "number" to (pack . show) cfg inp where
  to t = case double t of
    Right (d,"") -> round d
    _            -> _value inp


search :: Config m Search -> Input Search -> Html m (Input Search)
search = mkInput "search" coerce coerce


text :: forall m t. Coercible Text t => Config m t -> Input t -> Html m (Input t)
text = mkInput "text" coerce coerce
