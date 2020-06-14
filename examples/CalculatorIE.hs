{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}


module Main where


import           Control.Lens
import           Data.FileEmbed
import           Data.List                   as L
import           Data.List.Split             as L
import           Data.Text
import           Data.Text.Encoding
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html            as H

import           Debug.Trace


data Digit
  = Seven | Eight | Nine
  | Four  | Five  | Six
  | One   | Two   | Three
  | Zero deriving (Eq, Show, Ord, Enum, Bounded)


data Operator
  = Addition
  | Multiplication
  | Subtraction
  | Division
  deriving (Eq, Enum, Bounded)


instance Show Operator where
  show = \case
    Addition       -> "+"
    Subtraction    -> "-"
    Multiplication -> "×"
    Division       -> "÷"


data Entry = Whole [Digit] | [Digit] `Decimal` [Digit] | Negate Entry
  deriving Eq


noEntry :: Entry
noEntry = Whole []


instance Show Entry where
  show = let asChar = traverse . re charDigit in \case
    Whole xs        -> xs ^.. asChar
    xs `Decimal` ys -> xs ^.. asChar <> "." <> ys ^.. asChar
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
         [whole, dec] ->        (whole ^.. asChar)
                      `Decimal` (dec   ^.. asChar)
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


data State = Operation
  { _operator :: Operator
  , _entry    :: Entry
  } deriving (Eq, Show)


makeFieldsNoPrefix ''State


data Model = Model
  { _entry :: Entry
  , _state :: Maybe State
  } deriving (Eq, Show)


makeFieldsNoPrefix ''Model


initial :: Model
initial = Model noEntry Nothing


digit :: Digit -> Html' Digit
digit d = button [ onClick' d, className $ "d" <> d' ] [ text d' ]
  where d' = d ^. re charDigit . to (pack . pure)


operate :: Maybe Operator -> Operator -> Html' Operator
operate active o = button
  [ onClick' o, className ("active" :: Text, Just o == active) ]
  [ text . pack $ show o ]


applyDigit :: Digit -> Entry -> Entry
applyDigit d = \case
  Whole xs        -> Whole $ xs <> [d]
  xs `Decimal` ys -> xs `Decimal` (ys <> [d])
  Negate e        -> Negate (applyDigit d e)


addDecimal :: Entry -> Entry
addDecimal = \case
  Whole xs -> xs `Decimal` []
  ys       -> ys


calcResult :: Model -> Model
calcResult x = x
  & state .~ Nothing
  & entry .~ case x ^. state of
    Nothing -> x ^. entry
    Just o -> let l = o ^. entry . frac; r = x ^. entry . frac
      in (^. from frac) $ case o ^. operator of
      Addition       -> l + r
      Subtraction    -> l - r
      Multiplication -> l * r
      Division       -> l / r


neg :: Entry -> Entry
neg = \case
  Negate e -> e
  e -> Negate e


view :: Monad m => Model -> Html m Model
view x = H.div "calculator"
  [ H.div "readout" [ text . pack . show $ x ^. entry ]
  , ul "buttons"

    [ H.div "operate" $
      constly (\o x -> x & state .~ (Just (Operation o (x ^. entry)))
                         & entry .~ noEntry)
      . operate (x ^? state . traverse . operator) <$> [minBound .. maxBound]

    , H.div "numberpad" . L.intercalate [ br'_ ] . L.chunksOf 3 $
      constly putDigit . digit <$> [minBound .. pred maxBound]

    , H.div "zerodot"
      [ constly putDigit (digit Zero)
      , button [ onClick $ pur (& entry %~ addDecimal) ] [ "." ]
      ]


    , button [ class' "clear",  onClick (pur (const initial))  ] [ "C"   ]
    , button [ class' "posNeg", onClick (pur (& entry %~ neg)) ] [ "-/+" ]
    , button [ class' "equals", onClick (pur calcResult)       ] [ "="   ]
    ]
  ]

  where putDigit d = entry %~ applyDigit d


trapper :: Show a => (a -> Html m a) -> (a -> Html m a)
trapper v x = trace ("Trapper: " <> show x) $ v x


main :: IO ()
main = runJSorWarp 8080 $ do
  addInlineStyle $ decodeUtf8 $(embedFile "./CalculatorIE.css")
  Shpadoinkle.simple runParDiff initial (trapper Main.view) getBody
