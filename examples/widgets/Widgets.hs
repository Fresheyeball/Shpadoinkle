{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Monad.IO.Class            (liftIO)
import           Data.Text
import           Prelude                           hiding (div)

import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html                  as H (a, button, className,
                                                         div, div_, href, id',
                                                         link', rel,
                                                         textProperty, type')
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown
import           Shpadoinkle.Widgets.Types


default (Text)


data Cheese = Cheddar | Munster | Mozzeralla
  deriving (Show, Eq, Ord, Bounded, Enum)


instance Humanize Cheese where
  humanize = pack . show


instance Humanize (Maybe Cheese) where
  humanize = maybe "Please Select" humanize


data Model = Model
  { _pickOne        :: Dropdown 'One Cheese
  , _pickAtleastOne :: Dropdown 'AtleastOne Cheese
  } deriving (Eq, Show)


view :: MonadJSM m => Model -> Html m Model
view m = div_
  [ link' [ rel "stylesheet", href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" ]
  , liftMC (\m x -> m {_pickOne = x}) _pickOne $
    dropdown bootstrap defConfig { _attrs = [ id' "One" ] } (_pickOne m)
  , liftMC (\m x -> m {_pickAtleastOne = x}) _pickAtleastOne $
    dropdown bootstrap defConfig { _attrs = [ id' "AtleastOne" ] } (_pickAtleastOne m)
  ]
  where
  bootstrap Dropdown {..} = Dropdown.Theme
    { _wrapper = div
      [ className [ ("dropdown", True)
                  , ("show", _toggle == Open) ]
      ]
    , _header  = pure . button
      [ className ([ "btn", "btn-secondary", "dropdown-toggle" ] :: [Text])
      , type' "button"
      ]
    , _list    = div
      [ className [ ("dropdown-menu", True)
                  , ("show", _toggle == Open) ]
      ]
    , _item    = a [ className "dropdown-item"
                   , textProperty "style" "cursor:pointer" ]
    }



initial :: Model
initial = Model fullOptions $ minBound `withOptions'` fullset


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO initial
  shpadoinkle id runParDiff initial model view getBody


main :: IO ()
main = runJSorWarp 8080 app
