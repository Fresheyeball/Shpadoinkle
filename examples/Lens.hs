{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}


module Main where


import           Control.Lens                ((^.))
import           Data.Generics.Labels        ()
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack, unpack)
import           GHC.Generics                (Generic)
import           Safe                        (readMay)
import           Shpadoinkle                 (Html, JSM, runJSorWarp, simple,
                                              text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html            (button, div_, for', getBody, id',
                                              input', label, onClick, onInput,
                                              value)
import           Shpadoinkle.Lens            (onRecord, onSum)


data Form = Form
  { name :: Text
  , age  :: Int
  } deriving (Eq, Show, Generic)


form :: Functor m => Form -> Html m Form
form f = div_
  [ label [ for' "name" ] [ "Name" ]
  , onRecord #name $ input'
    [ id' "name"
    , value $ f ^. #name
    , onInput id
    ]
  , label [ for' "age" ] [ "Age" ]
  , onRecord #age $ input'
    [ id' "age"
    , value . pack . show $ f ^. #age
    , onInput $ fromMaybe 0 . readMay . unpack
    ]
  ]


newtype Counter = Counter Int
  deriving (Eq, Ord, Num, Show, Generic)


counter :: Counter -> Html m Counter
counter c = div_
  [ label [ for' "counter" ] [ text . pack $ show c ]
  , button [ id' "counter", onClick $ c + 1 ] [ "Increment" ]
  ]


data Model
  = MCounter Counter
  | MForm Form
  deriving (Eq, Show, Generic)


view :: Applicative m => Model -> Html m Model
view = \case
  MCounter c -> div_
    [ onSum #_MCounter $ counter c
    , button [ onClick . MForm $ Form "" 18 ] [ "Go to Form" ]
    ]
  MForm f    -> div_
    [ onSum #_MForm $ form f
    , button [ onClick $ MCounter 0 ] [ "Go to Counter" ]
    ]


app :: JSM ()
app = simple runParDiff (MCounter 0) view getBody


main :: IO ()
main = runJSorWarp 8080 app

