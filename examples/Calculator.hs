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


opSelect :: Html' Operation
opSelect = select [ onOption' $ read . unpack ]
  $ opOption <$> [minBound..maxBound]
  where opOption o = option [ value . pack $ show o ] [ text $ opText o ]


num :: Int -> Html' Int
num x = input'
 [ value . pack $ show x
 , onInput' $ fromMaybe 0 . readMay . unpack
 ]


view :: Monad m => Model -> Html m Model
view model = div_
 [ constly (set left) (num (_left model))
 , constly (set operation) opSelect
 , constly (set right) (num (_right model))
 , text $ " = " <> pack (show $ opFunction
     (_operation model) (_left model) (_right model))
 ]


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff (Model Addition 0 0) view getBody

