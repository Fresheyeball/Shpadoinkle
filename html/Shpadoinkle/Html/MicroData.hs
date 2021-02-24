{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Html.MicroData where


import           Data.Text
import           Shpadoinkle      (Html, Prop (PFlag))
import           Shpadoinkle.Html as H


itemscope :: (Text, Prop m a)
itemscope = ("itemscope", PFlag True)


itemtype :: Text -> (Text, Prop m a)
itemtype t = textProperty "itemtype" $ "https://schema.org/" <> t


propmeta :: Monad m => Text -> Text -> Html m a
propmeta ip c = meta' [ itemprop ip, content c ]


subitem :: Monad m => Text -> Text -> Text -> Text -> Html m a
subitem ip it ip' it' = H.div [ className "hidden", itemprop ip, itemscope, itemtype it ]
  [ propmeta ip' it' ]


