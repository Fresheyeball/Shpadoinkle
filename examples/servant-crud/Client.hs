{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Data.Text
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import qualified Shpadoinkle.Html            as H
import           Shpadoinkle.Html.Utils


import           Types


view :: () -> Html m ()
view _ = H.div_ . pure . text . pack $ show Foo


app :: JSM ()
app = fullPage runParDiff () view getBody


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
