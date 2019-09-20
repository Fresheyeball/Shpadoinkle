{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Monad.IO.Class           (liftIO)
import           Data.Text                        (pack)
import           Prelude                          hiding (span)
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp
#endif

import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html                 (br_', button, div_, h2_, id',
                                                   onClick, span, text)
import           Shpadoinkle.Html.Utils


view :: Applicative m => Int -> Html m Int
view count = div_
  [ h2_ [ "Counter Example" ]
  , "The current count is: "
  , span [ id' "out" ] [ text (pack $ show count) ]
  , br_', br_'
  , button [ onClick (count - 1) ] [ "Decrement" ]
  , button [ onClick (count + 1) ] [ "Increment" ]
  ]


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO 0
  shpadoinkle id runParDiff 0 model view getBody


main :: IO ()
#ifdef ghcjs_HOST_OS
main = app
#else
main = run 8080 app
#endif

