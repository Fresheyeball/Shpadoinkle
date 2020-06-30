{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Html.MicroData where


import           Data.Text
import           Shpadoinkle      (Html, Prop (PFlag))
import           Shpadoinkle.Html as H


itemscope :: (Text, Prop a)
itemscope = ("itemscope", PFlag True)


itemtype :: Text -> (Text, Prop a)
itemtype t = textProperty "itemtype" $ "https://schema.org/" <> t


propmeta :: Text -> Text -> Html a
propmeta ip c = meta' [ itemprop ip, content c ]


subitem :: Text -> Text -> Text -> Text -> Html a
subitem ip it ip' it' = H.div [ className "hidden", itemprop ip, itemscope, itemtype it ]
  [ propmeta ip' it' ]


