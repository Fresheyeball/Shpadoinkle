module Main where


import qualified Client
import qualified Server
import           Shpadoinkle.Run (Env (Dev), live)


main :: IO ()
main = live 8080 Client.app $ Server.application Dev "./static"
