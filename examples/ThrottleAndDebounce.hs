{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import           Control.Arrow               (first, second)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text, pack)
import           Shpadoinkle                 (Html, JSM, newTVarIO, shpadoinkle,
                                              text)
import           Shpadoinkle.Backend.ParDiff (ParDiffT, runParDiff)
import           Shpadoinkle.Html            (Debounce (..), Throttle (..),
                                              button, debounce, div_, getBody,
                                              input, onClick, onInput, throttle)
import           Shpadoinkle.Run             (runJSorWarp)


type Model = (Int, Text)


type App = ParDiffT Model JSM


data Control m = Control
  (Debounce m (Model -> Model) Model)
  (Debounce m (Text -> Model -> Model) Model)
  (Throttle m (Model -> Model) Model)
  (Throttle m (Text -> Model -> Model) Model)


view :: Control m -> Model -> Html m Model
view (Control debouncer1 debouncer2 throttler1 throttler2) (count, txt) = div_
  [ text ("Count: " <> pack (show count))
  , div_ [ button [ onClick $ first (+ 1) ] [ text "Increment" ] ]
  , div_ [ button [ runThrottle throttler1 onClick $ first (+ 1) ] [ text "Increment (throttle)" ] ]
  , div_ [ button [ runDebounce debouncer1 onClick $ first (+ 1) ] [ text "Increment (debounce)" ] ]
  , div_ [ text "Debounced input", input [ runDebounce debouncer2 onInput $ second . const ] [ ] ]
  , div_ [ text txt ]
  , div_ [ text "Throttled input", input [ runThrottle throttler2 onInput $ second . const ] [ ] ]
  ]


app :: Control App -> JSM ()
app control = do
  model <- liftIO $ newTVarIO initial
  shpadoinkle id runParDiff model (view control) getBody
  where initial = (0, "")


main :: IO ()
main = do
  control <- Control <$> debounce 1 <*> debounce 2 <*> throttle 1 <*> throttle 2
  runJSorWarp 8080 (app control)
