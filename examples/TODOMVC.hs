{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Natural
import           Language.Javascript.JSaddle
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html            (Html, h1_)


view :: Html m ()
view = h1_ [ "what!" ]

main :: IO ()
main = do
  model <- newTVarIO ()
  shpadoinkle (NT id) (runParDiffNT model) model (const view) . fmap RawNode $ liftIO $
    eval ("document.body" :: String)

