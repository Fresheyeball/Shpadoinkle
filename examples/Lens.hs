{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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
import           Shpadoinkle                 (Html, JSM, NFData, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html            (button, div_, for', getBody, id',
                                              input', label, onClick, onInput,
                                              value)
import           Shpadoinkle.Lens            (onRecord, onSum)
import           Shpadoinkle.Run             (run, simple)


data Form = Form
  { name :: Text
  , age  :: Int
  } deriving (Eq, Show, Generic, NFData)


form :: Applicative m => Form -> Html m Form
form f = div_
  [ label [ for' "name" ] [ "Name" ]
  , onRecord #name $ input'
    [ id' "name"
    , value $ f ^. #name
    , onInput const
    ]
  , label [ for' "age" ] [ "Age" ]
  , onRecord #age $ input'
    [ id' "age"
    , value . pack . show $ f ^. #age
    , onInput $ const . fromMaybe 0 . readMay . unpack
    ]
  ]


newtype Counter = Counter Int
  deriving stock Generic
  deriving newtype (Eq, Ord, Num, Show)
  deriving anyclass NFData


counter :: Counter -> Html m Counter
counter c = div_
  [ label [ for' "counter" ] [ text . pack $ show c ]
  , button [ id' "counter", onClick (+ 1) ] [ "Increment" ]
  ]


data Model
  = MCounter Counter
  | MForm Form
  deriving (Eq, Show, Generic, NFData)


view :: Applicative m => Model -> Html m Model
view = \case
  MCounter c -> div_
    [ onSum #_MCounter $ counter c
    , button [ onClick . const. MForm $ Form "" 18 ] [ "Go to Form" ]
    ]
  MForm f    -> div_
    [ onSum #_MForm $ form f
    , button [ onClick . const $ MCounter 0 ] [ "Go to Counter" ]
    ]


app :: JSM ()
app = simple runParDiff (MCounter 0) view getBody


main :: IO ()
main = run app
