{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where


import           Data.Maybe
import           Data.Text
import           Safe
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html


data Model = Model
  { operation :: Operation
  , left      :: Int
  , right     :: Int
  } deriving (Eq, Show)


data Operation
  = Addition
  | Subtraction
  | Multiplication
  | Division
  deriving (Eq, Show, Read, Enum, Bounded)


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


opSelect :: Html m Operation
opSelect = select [ onOption $ read . unpack ]
  $ opOption <$> [minBound..maxBound]
  where opOption o = option [ value . pack $ show o ] [ text $ opText o ]


num :: Int -> Html m Int
num x = input'
  [ value . pack $ show x
  , onInput $ fromMaybe 0 . readMay . unpack
  ]


view :: Functor m => Model -> Html m Model
view model = div_
  [ liftC (\l m -> m { left      = l }) left      $ num (left model)
  , liftC (\o m -> m { operation = o }) operation $ opSelect
  , liftC (\r m -> m { right     = r }) right     $ num (right model)
  , text $ " = " <> pack (show $ opFunction
      (operation model) (left model) (right model))
  ]


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff (Model Addition 0 0) view getBody

