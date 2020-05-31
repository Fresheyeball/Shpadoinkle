{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Main where


import           Data.Maybe
import           Data.Text
import           Safe
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html


data Model = Model
    { operation :: () -- Operation
    , left      :: Int
    , right     :: Int
    }
    deriving (Eq, Show)


data Operation = Addition
    | Subtraction
    | Multiplication
    | Division
    deriving (Eq, Show, Enum, Bounded)


opFunction :: Operation -> (Int -> Int -> Int)
opFunction = \case
  Addition       -> (+)
  Subtraction    -> (-)
  Multiplication -> (*)
  Division       -> \x y ->
    if y == 0 then 0 else Prelude.div x y


textOp :: Text -> Maybe Operation
textOp = \case
  "+" -> Just Addition
  "-" -> Just Subtraction
  "×" -> Just Multiplication
  "÷" -> Just Division
  _   -> Nothing


opText :: Operation -> Text
opText = \case
  Addition       -> "+"
  Subtraction    -> "-"
  Multiplication -> "×"
  Division       -> "÷"


opSelect :: MonadJSM m => Operation -> Html m Operation
opSelect op = select [ onChange (fromMaybe op . textOp) ] $ opOption <$> [minBound..maxBound]
  where opOption o = option [ value $ opText o ] [ text $ opText o ]


num :: MonadJSM m => Int -> Html m Int
num x = input'
 [ value . pack $ show x
 , onInput (fromMaybe 0 . readMay . unpack)
 ]


view :: MonadJSM m => Model -> Html m Model
view model = div_
 [ (\l -> model { left      = l }) <$> num (left model)
 , (\o -> model { operation = o }) <$> opSelect (operation model)
 , (\r -> model { right     = r }) <$> num (right model)
 , text $ " = " <> pack (show $ opFunction
     (operation model) (left model) (right model))
 ]


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff (Model Addition 0 0) view getBody

