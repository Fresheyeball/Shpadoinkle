{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Route
  ( Location(..)
  , getLocation
  ) where


import           Control.Isomorphic
import           Control.Lens
import           GHCJS.DOM.Types (JSM)
import           Language.Javascript.JSaddle hiding (JSM)
import           Network.HTTP.Types.URI


default (Text)


newtype Location = Location { unLocation :: Text }


getLocation :: JSM Location
getLocation = fmap Location . fromJSValUnchecked =<< jsg "window" ^. js "location" ^. js "href"
