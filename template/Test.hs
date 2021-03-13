{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where


import           Data.FileEmbed
import           Data.Text
import           Data.Text.Encoding
import           Shpadoinkle
import           Shpadoinkle.Backend.Static
import           Shpadoinkle.Template
import           Shpadoinkle.Template.TH


testHtmlIngestion :: IO ()
testHtmlIngestion =
  let x = mconcat $ renderStatic <$> $(embedHtml "./test.html")
      y = decodeUtf8 $(embedFile "./test.html")
  in if x == y then pure () else
     error $ "test.html did not parse correctly. Got: " ++ unpack x

testTemplate :: IO ()
testTemplate =
  let x = renderStatic $ template (replace "{{x}}" "yoddle") $
            h "div{{x}}" [ ("{{x}}class", textProp "bar{{x}}") ]
              [ h "span" [] [ "Hi {{x}}" ]
              ]
      y = "<divyoddle yoddleclass=\"baryoddle\"><span>Hi yoddle</span></divyoddle>"
  in if x == y then pure () else
     error $ "template did not interpolate. Got: " ++ unpack x

main :: IO ()
main = do
  testHtmlIngestion
  testTemplate
  putStrLn "SUCCESS"
