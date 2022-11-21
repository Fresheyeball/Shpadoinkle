{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Shpadoinkle                  (Html, JSM)
import           Shpadoinkle.Backend.Snabbdom (runSnabbdom, stage)
import           Shpadoinkle.Html
import           Shpadoinkle.Run              (run, simple)


view :: () -> Html m ()
view _ = div_ [ "hello world" ]


app :: JSM ()
app = simple runSnabbdom () view stage


main :: IO ()
main = do
  putStrLn "\nhi, my name is snowman"
  putStrLn "happy point of view on http://localhost:8080\n"
  run app
