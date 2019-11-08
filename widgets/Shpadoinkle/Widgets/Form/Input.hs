{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Widgets.Form.Input where


import           Data.Text

import           Shpadoinkle
import           Shpadoinkle.Html               as Html
import           Shpadoinkle.Widgets.Types.Core
import           Shpadoinkle.Widgets.Types.Form as Form


data Config m a = Config
  { _placeholder :: Placeholder
  , _attrs       :: Props m (Input a)
  }


emptyCfg :: Config m a
emptyCfg = Config (Placeholder "") []


mkInput :: MonadJSM m => Text -> (Text -> a) -> (a -> Text) -> Config m a -> Input a -> Html m (Input a)
mkInput t to from (Config (Placeholder ph) attrs) inp = Html.input
  ( Html.value (from $ Form.value inp)
  : Html.placeholder ph
  : Html.onInput (Input Dirty . to)
  : Html.type' t
  : attrs ) []


text :: MonadJSM m => Config m Text -> Input Text -> Html m (Input Text)
text = mkInput "text" id id


search :: MonadJSM m => Config m Search -> Input Search -> Html m (Input Search)
search = mkInput "search" Search unSearch
