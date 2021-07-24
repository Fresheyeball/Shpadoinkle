{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}


module Main where


import           Prelude                     hiding (div)

import           Control.Concurrent          (threadDelay)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Text                   (Text, pack)
import           Shpadoinkle                 (Html, JSM, NFData, liftC)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html            (button, div, getBody, onClickC,
                                              text)
import           Shpadoinkle.Run             (runJSorWarp, simple, live)
import           Shpadoinkle.Streaming       (consumeStream)
import           "streaming" Streaming       (Of, Stream)
import           Streaming.Prelude           (repeatM)

default (Text)


exampleStream :: MonadIO m => Stream (Of Int) m ()
exampleStream = repeatM $ do
  liftIO $ threadDelay 1000000
  return 1


newtype Model = Model { streamContents :: [Int] }
  deriving (Eq, Show, NFData)


view :: MonadIO m => Model -> Html m Model
view (Model ns) =
  div
    []
    [ text (pack (show ns))
    , liftC (\c m -> m { streamContents = c }) streamContents $
      button
        [ onClickC (consumeStream exampleStream (return . (:))) ]
        [ text "Go" ]
    ]


app :: JSM ()
app = simple runParDiff (Model []) view getBody


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = runJSorWarp 8080 app
