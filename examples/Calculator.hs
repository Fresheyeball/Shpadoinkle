{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff () (const "hello world") getBody
