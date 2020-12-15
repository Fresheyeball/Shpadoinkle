{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Concurrent                (threadDelay)
import           Control.Lens                      hiding (view)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Text                         (Text, pack)
import           Prelude                           hiding (div)

import           Shpadoinkle                       (Html, JSM, MonadJSM,
                                                    liftJSM, newTVarIO,
                                                    shpadoinkle)
-- import           Shpadoinkle.Backend.ParDiff       (runParDiff)
import           Shpadoinkle.Backend.Snabbdom      (runSnabbdom)
import           Shpadoinkle.Html                  as H (a, button, class', div,
                                                         div_, href, id', link',
                                                         onClick, onClickM, rel,
                                                         text, textProperty,
                                                         type')
import           Shpadoinkle.Html.Utils            (getBody)
import           Shpadoinkle.Lens                  (onRecord)
import           Shpadoinkle.Run                   (runJSorWarp)
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown
import           Shpadoinkle.Widgets.Types         (Humanize (..), Pick (..),
                                                    Present (..), Selected,
                                                    Toggle (..), fullOptions,
                                                    fullset, withOptions')


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
  , _concTest       :: Int
  } deriving (Eq, Show)
makeLenses ''Model


conc :: MonadJSM m => Int -> Html m Int
conc x = div_
  [ text . pack $ show x
  , button
    [ onClick (+ 1)
    , onClickM . liftJSM $ do
      liftIO $ threadDelay 3000000
      return (* 3)
    ] [ text "TEST" ]
  ]


view :: MonadJSM m => Model -> Html m Model
view m = div_
  [ link' [ rel "stylesheet", href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" ]
  , onRecord pickOne        $ dropdown bootstrap defConfig { _attrs = [ id' "One" ] } (_pickOne m)
  , onRecord pickAtleastOne $ dropdown bootstrap defConfig { _attrs = [ id' "AtleastOne" ] } (_pickAtleastOne m)
  , onRecord concTest $ conc $ _concTest m
  ]
  where
  bootstrap :: Present b => Present (Selected p b) => Dropdown p b -> Theme m p b
  bootstrap Dropdown {..} = Dropdown.Theme
    { _wrapper = div
      [ class' [ ("dropdown", True)
               , ("show", _toggle == Open) ]
      ]
    , _header  = pure . button
      [ class' ([ "btn", "btn-secondary", "dropdown-toggle" ] :: [Text])
      , type' "button"
      ]
      . present
    , _list    = div
      [ class' [ ("dropdown-menu", True)
               , ("show", _toggle == Open) ]
      ]
    , _item    = a [ class' "dropdown-item"
                   , textProperty "style" "cursor:pointer" ]
                 . present
    }


initial :: Model
initial = Model fullOptions (minBound `withOptions'` fullset) 4


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO initial
  shpadoinkle id runSnabbdom initial model view getBody


main :: IO ()
main = runJSorWarp 8080 app
