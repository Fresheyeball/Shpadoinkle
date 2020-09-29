{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (pack)
import           Prelude                     hiding (span)

import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html            (br'_, button, div_, h2_, id',
                                              onClick, span)
import           Shpadoinkle.Html.Utils


view :: Int -> Html m Int
view count = div_
  [ h2_ [ "Counter Example" ]
  , "The current count is: "
  , span [ id' "out" ] [ text (pack $ show count) ]
  , br'_, br'_
  , button [ onClick $ count - 1 ] [ "Decrement" ]
  , button [ onClick $ count + 1 ] [ "Increment" ]
  ]


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO 0
  shpadoinkle id runParDiff 0 model view getBody


main :: IO ()
main = runJSorWarp 8080 app

