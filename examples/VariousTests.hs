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

import Data.Function ((&))
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (decodeUtf8)
import           GHC.Generics                (Generic)
import           Shpadoinkle                 (Html, JSM, NFData, liftC)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Console         (ToJSON, ToJSVal, askJSM, logJS,
                                              trapper)
import           Shpadoinkle.Html            as H
import Data.Semigroup (stimes)
import Shpadoinkle.Html.Event (onKeydown, onClickAway, onGlobalKeyDown, onGlobalKeyDownNoRepeat, onEscape)
import Shpadoinkle.JSFFI (console, (#-), liftJSM, MonadJSM)
import           Shpadoinkle.Run             (run, simple)


data Model = Model
  deriving (Show, Generic, NFData, Eq)

initial :: Model
initial = Model


view :: MonadJSM m => Model -> Html m Model
view m = H.div "calculator"
  [ H.div [] [ H.text "hello, there!" ]
  , H.div []
    ([ H.input
      [ H.type' "text"
      , onClickAwayM             (        logIt "[input] click away")
      , onKeydownM               (const $ logIt "[input] key down")
      , onGlobalKeyDownM         (const $ logIt "[input] global key down")
      -- , onGlobalKeyDownNoRepeatM (const $ logIt "[input] global key down (no repeat)")
      -- nb. Both onGlobalKeyDown and onGlobalKeyDownNoRepeat seem to work, but not together
      , onEscapeM                (        logIt "[input] escape")
      ]
      []
    , H.text " "
    ] & stimes 2)
  ]

  where

  logIt str = liftJSM $ id <$ (console #- "log" $ [str])


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
