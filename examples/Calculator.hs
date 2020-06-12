{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where


import           Control.Lens                hiding (simple, view)
import           Data.Maybe
import           Data.Text
import           Safe
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html
import           Shpadoinkle.Lens


data Model = Model
    { _operation :: Operation
    , _left      :: Int
    , _right     :: Int
    }
    deriving (Eq, Show)


data Operation = Addition
    | Subtraction
    | Multiplication
    | Division
    deriving (Eq, Show, Read, Enum, Bounded)


makeLenses ''Model


opFunction :: Operation -> (Int -> Int -> Int)
opFunction = \case
  Addition       -> (+)
  Subtraction    -> (-)
  Multiplication -> (*)
  Division       -> \x y ->
    if y == 0 then 0 else Prelude.div x y


opText :: Operation -> Text
opText = \case
  Addition       -> "+"
  Subtraction    -> "-"
  Multiplication -> "×"
  Division       -> "÷"


opSelect :: MonadJSM m => Html m Operation
opSelect = select [ onOption $ pur . const . read . unpack ]
  $ opOption <$> [minBound..maxBound]
  where opOption o = option [ value . pack $ show o ] [ text $ opText o ]


num :: MonadJSM m => Int -> Html m Int
num x = input'
 [ value . pack $ show x
 , onInput $ pur . const . fromMaybe 0 . readMay . unpack
 ]


view :: MonadJSM m => Model -> Html m Model
view model = div_
 [ generalize left (num (_left model))
 , generalize operation opSelect
 , generalize right (num (_right model))
 , text $ " = " <> pack (show $ opFunction
     (_operation model) (_left model) (_right model))
 ]


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff (Model Addition 0 0) view getBody

