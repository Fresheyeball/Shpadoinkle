{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults       #-}

module Main where

import           Control.Lens                (Iso', Prism', from, iso,
                                              makeFieldsNoPrefix, prism', re,
                                              to, (%~), (&), (.~), (?~), (^.),
                                              (^..), (^?))
import           Data.Aeson                  (ToJSON)
import           Data.FileEmbed              (embedFile)
import           Data.List                   as L
import           Data.List.Split             as L
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (decodeUtf8)
import           GHC.Generics                (Generic)
import           Shpadoinkle                 (Html, JSM, NFData, liftC)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Console         (askJSM, trapper)
import           Shpadoinkle.Html            as H
import           Shpadoinkle.Run             (live, runJSorWarp, simple)

default (ClassList)

data Digit
  = Seven | Eight | Nine
  | Four  | Five  | Six
  | One   | Two   | Three
  | Zero deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON, NFData)

data Operator = Addition | Multiplication | Subtraction | Division
  deriving (Eq, Enum, Bounded, Generic, ToJSON, NFData)

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
  deriving (Eq, Generic, ToJSON, NFData)

noEntry :: Entry
noEntry = Whole []

instance Show Entry where
  show = let asChar = traverse . re charDigit in \case
    Whole xs   -> xs ^.. asChar
    xs :<.> ys -> xs ^.. asChar <> "." <> ys ^.. asChar
    Negate e   -> '-' : show e

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
  , _previous :: Entry
  } deriving (Eq, Show, Generic, ToJSON, NFData)

makeFieldsNoPrefix ''Operation

data Model = Model
  { _current   :: Entry
  , _operation :: Maybe Operation
  } deriving (Eq, Show, Generic, ToJSON, NFData)

makeFieldsNoPrefix ''Model

initial :: Model
initial = Model noEntry Nothing

digit :: Digit -> Html m Model
digit d = button [ onClick $ current %~ applyDigit d, class' $ "d" <> d' ] [ text d' ]
  where d' = d ^. re charDigit . to (pack . pure)

operate :: Maybe Operator -> Operator -> Html m Operator
operate active o = button
  [ onClick $ const o, class' ("active" :: Text, Just o == active) ]
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
  & current .~ case x ^. operation of
    Nothing -> x ^. current
    Just o -> let l = o ^. previous . frac; r = x ^. current . frac
      in cleanEntry . (^. from frac) $ case o ^. operator of
      Addition       -> l + r
      Subtraction    -> l - r
      Multiplication -> l * r
      Division       -> if r == 0 then l else l / r

neg :: Entry -> Entry
neg = \case
  Negate e -> e
  e        -> Negate e

readout :: Model -> Html m a
readout x = H.div "readout" $
  [ text . pack . show $ x ^. current
  ] <> case x ^? operation . traverse . operator of
         Nothing -> []
         Just o  -> [ H.div "operator"
             [ text $ pack (show o) <> " " <>
               x ^. operation . traverse . previous . to (pack .show)
             ]
           ]

clear :: Html m Model
clear  = button [ class' "clear", onClick $ const initial ] [ "AC" ]

posNeg :: Html m Model
posNeg = button [ class' "posNeg", onClick $ current %~ neg ] [ "-/+" ]

numberpad :: Html m Model
numberpad = H.div "numberpad" . L.intercalate [ br'_ ] . L.chunksOf 3 $
  digit <$> [minBound .. pred maxBound]

operations :: Functor m => Model -> Html m Model
operations x = H.div "operate" $ (\o' -> liftC (\o _ -> x
  & operation ?~ Operation o (x ^. current)
  & current .~ noEntry) (const o')
  $ operate (x ^? operation . traverse . operator) o') <$> ([minBound .. maxBound] :: [Operator])

dot :: Html m Model
dot = button [ onClick $ current %~ addDecimal ] [ "." ]

equals :: Html m Model
equals = button [ class' "equals", onClick calcResult ] [ "=" ]

view :: Functor m => Model -> Html m Model
view m = H.div "calculator"
  [ readout m
  , H.div "buttons"
    [ clear, posNeg, operations m
    , numberpad
    , H.div "zerodot"
      [ digit Zero
      , dot
      , equals
      ]
    ]
  ]


app :: JSM ()
app = do
  setTitle "Calculator"
  ctx <- askJSM
  addInlineStyle $ decodeUtf8 $(embedFile "./CalculatorIE.css")
  simple runParDiff initial (Main.view . trapper @ToJSON ctx) getBody


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = runJSorWarp 8080 app
