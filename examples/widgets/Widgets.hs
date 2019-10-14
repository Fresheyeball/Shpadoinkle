{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Monad.IO.Class            (liftIO)
import           Data.Text
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp
#endif

import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html                  (div_, href, id', link', rel)
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown
import           Shpadoinkle.Widgets.Types


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
  , (\x -> m {_pickOne = x}) <$>
    dropdown bootstrap defConfig { _attrs = [ id' "One" ] } (_pickOne m)
  , (\x -> m {_pickAtleastOne = x}) <$>
    dropdown bootstrap defConfig { _attrs = [ id' "AtleastOne" ] } (_pickAtleastOne m)
  ]


initial :: Model
initial = Model fullOptions $ minBound `withOptions'` fullset


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO initial
  shpadoinkle id runParDiff initial model view getBody


main :: IO ()
#ifdef ghcjs_HOST_OS
main = app
#else
main = run 8080 app
#endif
