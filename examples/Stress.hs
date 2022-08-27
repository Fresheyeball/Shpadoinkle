{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Data.Text                    (Text, pack)
import           Shpadoinkle                  (Html, JSM)
import           Shpadoinkle.Backend.Snabbdom (runSnabbdom, stage)
-- import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html             (div_, input', onInput, text,
                                               value)
import           Shpadoinkle.Run              (live, runJSorWarp, simple)


view :: Text -> Html m Text
view x = div_
  [ input' [ onInput const, value x ]
  , div_ $ (\y -> div_ [ text $ pack (show y) <> x ]) <$> [(0::Int)..500]
  ]


app :: JSM ()
app = simple runSnabbdom "" view stage


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = runJSorWarp 8080 app
