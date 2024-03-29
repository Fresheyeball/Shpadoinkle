{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Main where


import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack, unpack)
import           GHC.Generics                (Generic)
import           Safe                        (readMay)
import           Shpadoinkle                 (Html, JSM, NFData, liftC, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html            (div_, getBody, input', onInput,
                                              onOption, option, select, value)
import           Shpadoinkle.Run             (run, simple)


data Model = Model
  { operation :: Operation
  , left      :: Int
  , right     :: Int
  } deriving (Eq, Show, Generic, NFData)


data Operation
  = Addition
  | Subtraction
  | Multiplication
  | Division
  deriving (Eq, Show, Read, Enum, Bounded, Generic, NFData)


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
opSelect = select [ onOption $ const . read . unpack ]
  $ opOption <$> [minBound..maxBound]
  where opOption o = option [ value . pack $ show o ] [ text $ opText o ]


num :: Int -> Html m Int
num x = input'
  [ value . pack $ show x
  , onInput $ const . fromMaybe 0 . readMay . unpack
  ]


view :: Functor m => Model -> Html m Model
view model = div_
  [ liftC (\l m -> m { left      = l }) left    $ num (left model)
  , liftC (\o m -> m { operation = o }) operation opSelect
  , liftC (\r m -> m { right     = r }) right   $ num (right model)
  , text $ " = " <> pack (show $ opFunction
      (operation model) (left model) (right model))
  ]


app :: JSM ()
app = simple runParDiff (Model Addition 0 0) view getBody


main :: IO ()
main = run app
