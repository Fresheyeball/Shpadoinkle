{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Main where


import Control.Arrow ( second )
import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Shpadoinkle
import Shpadoinkle.Backend.ParDiff
import Shpadoinkle.Html


type Model = (Int, Text)

type App = ParDiffT Model JSM

data Control m = Control
               { _debounceModel :: Debounce m Model Model
               , _debounceInput :: Debounce m (Text -> Continuation m Model) Model
               , _throttleModel :: Throttle m Model Model
               , _throttleInput :: Throttle m (Text -> Continuation m Model) Model }


view :: Monad m
     => Control m
     -> Model
     -> HtmlM m Model
view (Control (Debounce debounceModel) (Debounce debounceInput) (Throttle throttleModel) (Throttle throttleInput))
     (count, txt) = div_
  [ text ("Count: " <> pack (show count))
  , div_ [ button [ onClick $ (count+1, txt) ] [ text "Increment" ] ]
  , div_ [ button [ throttleModel onClick $ (count+1, txt) ] [ text "Increment (throttle)" ] ]
  , div_ [ button [ debounceModel onClick $ (count+1, txt) ] [ text "Increment (debounce)" ] ]
  , div_ [ text "Debounced input", input [ debounceInput onInputE (pur . second . const) ] [ ] ]
  , div_ [ text txt ]
  , div_ [ text "Throttled input", input [ throttleInput onInputE (pur . second . const) ] [ ] ]
  ]


app :: Control App -> JSM ()
app control = do
  model <- liftIO $ newTVarIO initial
  shpadoinkle id runParDiff initial model (view control) getBody
  where initial = (0, "")


main :: IO ()
main = do
  control <- Control <$> (Debounce <$> debounce 1)
                     <*> (Debounce <$> debounce 2)
                     <*> (Throttle <$> throttle 1)
                     <*> (Throttle <$> throttle 2)
  runJSorWarp 8080 (app control)
