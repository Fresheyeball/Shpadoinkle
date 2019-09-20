module Main where


import           Test.Hspec

import           Spec       (spec)


main :: IO ()
main = hspec spec >> putStrLn "SUCCESS"
