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


type Config m a = Props m (Input a)


mkInput :: MonadJSM m => Text -> (Text -> a) -> (a -> Text) -> Config m a -> Input a -> Html m (Input a)
mkInput t to from attrs inp = Html.input
  ( Html.value (from $ Form._value inp)
  : Html.onInput (Input Dirty . to)
  : Html.type' t
  : attrs ) []


fractional :: MonadJSM m => Fractional n => Show n => Config m n -> Input n -> Html m (Input n)
fractional cfg inp = mkInput "number" to (pack . show) cfg inp where
  to t = case double t of
    Right (d,"") -> realToFrac d
    _            -> _value inp


integral :: MonadJSM m => Integral n => Show n => Config m n -> Input n -> Html m (Input n)
integral cfg inp = mkInput "number" to (pack . show) cfg inp where
  to t = case double t of
    Right (d,"") -> round d
    _            -> _value inp


search :: MonadJSM m => Config m Search -> Input Search -> Html m (Input Search)
search = mkInput "search" coerce coerce


text :: forall m t. (MonadJSM m, Coercible Text t) => Config m t -> Input t -> Html m (Input t)
text = mkInput "text" coerce coerce
