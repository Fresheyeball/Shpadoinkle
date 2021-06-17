{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Shpadoinkle                  (Html, JSM)
import           Shpadoinkle.Backend.Snabbdom (runSnabbdom, stage)
import           Shpadoinkle.Html
import           Shpadoinkle.Run              (live, runJSorWarp, simple)


view :: () -> Html m ()
view _ = div_ [ "hello world" ]


app :: JSM ()
app = simple runSnabbdom () view stage


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  putStrLn "\nhi, my name is snowman"
  putStrLn "happy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app
