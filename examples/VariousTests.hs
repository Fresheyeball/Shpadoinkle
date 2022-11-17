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

import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (decodeUtf8)
import           GHC.Generics                (Generic)
import           Shpadoinkle                 (Html, JSM, NFData, liftC)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Console         (ToJSON, ToJSVal, askJSM, logJS,
                                              trapper)
import           Shpadoinkle.Html            as H
import           Shpadoinkle.Run             (run, simple)


data Model = Model
  deriving (Show, Generic, NFData, Eq)

initial :: Model
initial = Model


view :: Functor m => Model -> Html m Model
view m = H.div "calculator"
  [ H.text "hello, there!"
  ]


app :: JSM ()
app = do
  setTitle "Calculator"
  ctx <- askJSM

  logJS @Show "info" ["hello", "there"]
  logJS @Show "warn" ["hello", "here"]
  logJS @ToJSON "info" ["hello", "there"]
  logJS @ToJSON "warn" ["hello", "here"]
  logJS @ToJSVal "info" ("hello / there" :: Text)
  logJS @ToJSVal "warn" ("hello / here" :: Text)

  simple runParDiff initial (view . trapper @Show ctx) getBody


main :: IO ()
main = run app
