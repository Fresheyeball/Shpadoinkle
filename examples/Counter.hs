{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Natural
import           Data.Text                   (pack)
import           Language.Javascript.JSaddle
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html            (br_', button, div_, h2_, onClick,
                                              text)


view :: Applicative m => Int -> Html m Int
view count = div_
  [ h2_ [ "Counter Example" ]
  , "The current count is: "
  , text (pack $ show count)
  , br_', br_'
  , button [ onClick (count - 1) ] [ "Decrement" ]
  , button [ onClick (count + 1) ] [ "Increment" ]
  ]


main :: IO ()
main = do
  model <- newTVarIO 0
  shpadoinkle (NT id) (runParDiffNT model) model view . liftIO $
    RawNode <$> eval ("document.body" :: String)

