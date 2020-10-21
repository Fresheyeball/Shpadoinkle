module Main where


import qualified Client
import           Shpadoinkle.Run (runJSorWarp)


main :: IO ()
main = runJSorWarp 8080 Client.app
