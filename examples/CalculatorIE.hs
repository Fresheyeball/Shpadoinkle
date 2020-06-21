{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Main where

import           Control.Lens
import           Data.Aeson
import           Data.FileEmbed
import           Data.List                   as L
import           Data.List.Split             as L
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics                (Generic)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Debug
import           Shpadoinkle.Html            as H

data Digit
  = Seven | Eight | Nine
  | Four  | Five  | Six
  | One   | Two   | Three
  | Zero deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON)

data Operator = Addition | Multiplication | Subtraction | Division
  deriving (Eq, Enum, Bounded, Generic, ToJSON)

instance Show Operator where
  show = \case
    Addition       -> "+"
    Subtraction    -> "−"
    Multiplication -> "×"
    Division       -> "÷"

data Entry
  = Whole [Digit]
  | [Digit] :<.> [Digit]
  | Negate Entry
  deriving (Eq, Generic, ToJSON)

noEntry :: Entry
noEntry = Whole []

instance Show Entry where
  show = let asChar = traverse . re charDigit in \case
    Whole xs        -> xs ^.. asChar
    xs :<.> ys -> xs ^.. asChar <> "." <> ys ^.. asChar
    Negate e        -> '-' : show e

frac :: Iso' Entry Double
frac = iso toFrac fromFrac where
  toFrac =
    let f x = case L.reverse x of
             ""     -> "0"
             '.':xs -> L.reverse xs
             _      -> x
    in read . f . show
  fromFrac d =
    let asChar = traverse . charDigit
        ab xs = case L.splitOn "." xs of
         [whole, []]  -> Whole $ whole ^.. asChar
         [whole, dec] ->   (whole ^.. asChar)
                      :<.> (dec   ^.. asChar)
         _            -> noEntry
     in case show d of
       '-':xs -> Negate $ ab xs
       xs     -> ab xs

charDigit :: Prism' Char Digit
charDigit = prism'
  (\case Seven    -> '7';   Eight ->    '8';   Nine  ->   '9'
         Four     -> '4';   Five  ->    '5';   Six   ->   '6'
         One      -> '1';   Two   ->    '2';   Three ->   '3'
         Zero     -> '0')
  (\case '7' -> Just Seven; '8' -> Just Eight; '9' -> Just Nine
         '4' -> Just Four;  '5' -> Just Five;  '6' -> Just Six
         '1' -> Just One;   '2' -> Just Two;   '3' -> Just Three
         '0' -> Just Zero;   _  -> Nothing)

data Operation = Operation
  { _operator :: Operator
  , _entry    :: Entry
  } deriving (Eq, Show, Generic, ToJSON)

makeFieldsNoPrefix ''Operation

data Model = Model
  { _entry     :: Entry
  , _operation :: Maybe Operation
  } deriving (Eq, Show, Generic, ToJSON)

makeFieldsNoPrefix ''Model

initial :: Model
initial = Model noEntry Nothing

digit :: Digit -> Html Digit
digit d = button [ onClick d, className $ "d" <> d' ] [ text d' ]
  where d' = d ^. re charDigit . to (pack . pure)

operate :: Maybe Operator -> Operator -> Html Operator
operate active o = button
  [ onClick o, className ("active" :: Text, Just o == active) ]
  [ text . pack $ show o ]

applyDigit :: Digit -> Entry -> Entry
applyDigit d = \case
  Whole xs   -> Whole $ xs <> [d]
  xs :<.> ys -> xs :<.> (ys <> [d])
  Negate e   -> Negate $ applyDigit d e

addDecimal :: Entry -> Entry
addDecimal = \case
  Whole xs -> xs :<.> []
  ys       -> ys

cleanEntry :: Entry -> Entry
cleanEntry = \case
  xs :<.> []     -> Whole xs
  xs :<.> [Zero] -> Whole xs
  x              -> x

calcResult :: Model -> Model
calcResult x = x
  & operation .~ Nothing
  & entry .~ case x ^. operation of
    Nothing -> x ^. entry
    Just o -> let l = o ^. entry . frac; r = x ^. entry . frac
      in cleanEntry . (^. from frac) $ case o ^. operator of
      Addition       -> l + r
      Subtraction    -> l - r
      Multiplication -> l * r
      Division       -> if r == 0 then l else l / r

neg :: Entry -> Entry
neg = \case
  Negate e -> e
  e -> Negate e

view :: Model -> Html Model
view x = H.div "calculator"
  [ H.div "readout" $
    [ text . pack . show $ x ^. entry
    ] <> case x ^? operation . traverse . operator of
           Nothing -> []
           Just o  -> [ H.div "operator"
               [ text $ pack (show o) <> " " <>
                 x ^. operation . traverse . entry . to (pack .show)
               ]
             ]
  , H.div "buttons"

    [ button [ class' "clear", onClick initial ] [ "AC" ]
    , button [ class' "posNeg", onClick (x & entry %~ neg) ] [ "-/+" ]
    , H.div "operate" $ fmap (\o -> x
      & operation .~ Just (Operation o (x ^. entry))
      & entry .~ noEntry)
      . operate (x ^? operation . traverse . operator) <$> [minBound .. maxBound]

    , H.div "numberpad" . L.intercalate [ br'_ ] . L.chunksOf 3 $
       fmap putDigit . digit <$> [minBound .. pred maxBound]

    , H.div "zerodot"
      [ putDigit <$> digit Zero
      , button [ onClick $ x & entry %~ addDecimal ] [ "." ]
      , button [ class' "equals", onClick $ calcResult x ] [ "=" ]
      ]
    ]
  ] where putDigit d = x & entry %~ applyDigit d

main :: IO ()
main = runJSorWarp 8080 $ do
  setTitle "Calculator"
  ctx <- askJSM
  addInlineStyle $ decodeUtf8 $(embedFile "./CalculatorIE.css")
  Shpadoinkle.simple runParDiff initial (constly' . Main.view . trapper @ToJSON ctx) getBody
