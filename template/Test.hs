{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where


import           Data.FileEmbed
import           Data.Text
import           Data.Text.Encoding
import           Shpadoinkle
import           Shpadoinkle.Backend.Static
import           Shpadoinkle.Template.TH


main :: IO ()
main =
  let x = mconcat $ renderStatic <$> $(embedHtml "./test.html")
      y = decodeUtf8 $(embedFile "./test.html")
  in if x == y
  then putStrLn "SUCCESS"
  else error $ "test.html did not parse correctly. Got: " ++ unpack x
