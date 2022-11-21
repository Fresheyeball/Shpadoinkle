module Main where


import qualified Client
import           Shpadoinkle.Run (run)


main :: IO ()
main = run Client.app
