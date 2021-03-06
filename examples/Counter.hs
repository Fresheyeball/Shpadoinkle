{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Data.Text                   (pack)
import           Prelude                     hiding (span)

import           Shpadoinkle                 (Html, JSM, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html            (br'_, button, div_, h2_, id',
                                              onClick, span)
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Run             (runJSorWarp, simple)


view :: Int -> Html m Int
view count = div_
  [ h2_ [ "Counter Example" ]
  , "The current count is: "
  , span [ id' "out" ] [ text . pack $ show count ]
  , br'_, br'_
  , button [ onClick $ subtract 1 ] [ "Decrement" ]
  , button [ onClick (+ 1)        ] [ "Increment" ]
  ]


app :: JSM ()
app = simple runParDiff 0 view getBody


main :: IO ()
main = runJSorWarp 8080 app

