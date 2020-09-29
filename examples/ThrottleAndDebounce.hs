{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Control.Monad.IO.Class
import           Data.Text                   (Text, pack)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html


type Model = (Int, Text)


type App = ParDiffT Model JSM


data Control m = Control
  (Debounce m Model Model)
  (Debounce m (Text -> Model) Model)
  (Throttle m Model Model)
  (Throttle m (Text -> Model) Model)


view :: Control m -> Model -> Html m Model
view (Control debouncer1 debouncer2 throttler1 throttler2) (count, txt) = div_
  [ text ("Count: " <> pack (show count))
  , div_ [ button [ onClick (count+1, txt) ] [ text "Increment" ] ]
  , div_ [ button [ runThrottle throttler1 onClick (count+1, txt) ] [ text "Increment (throttle)" ] ]
  , div_ [ button [ runDebounce debouncer1 onClick (count+1, txt) ] [ text "Increment (debounce)" ] ]
  , div_ [ text "Debounced input", input [ runDebounce debouncer2 onInput (count,) ] [ ] ]
  , div_ [ text txt ]
  , div_ [ text "Throttled input", input [ runThrottle throttler2 onInput (count,) ] [ ] ]
  ]


app :: Control App -> JSM ()
app control = do
  model <- liftIO $ newTVarIO initial
  shpadoinkle id runParDiff initial model (view control) getBody
  where initial = (0, "")


main :: IO ()
main = do
  control <- Control <$> debounce 1 <*> debounce 2 <*> throttle 1 <*> throttle 2
  runJSorWarp 8080 (app control)
