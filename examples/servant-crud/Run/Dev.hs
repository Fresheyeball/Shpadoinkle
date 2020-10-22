module Main where


import qualified Client
import qualified Server
import           Shpadoinkle.Run (Env (Dev), liveWithBackend)


main :: IO ()
main = liveWithBackend 8080 Client.app $ Server.application Dev "./static"
