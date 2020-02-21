{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module View where


import           Control.Lens                      hiding (view)
import           Data.Text                         as T
import           Shpadoinkle
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Lens
import           Shpadoinkle.Router                (navigate, toHydration)
import           Shpadoinkle.Widgets.Form.Dropdown
import qualified Shpadoinkle.Widgets.Form.Input    as Input
import           Shpadoinkle.Widgets.Table         as Table
import           Shpadoinkle.Widgets.Types
import           Shpadoinkle.Widgets.Types.Form    as Form

import           Types


toEditForm :: SpaceCraft -> EditForm
toEditForm sc = EditForm
  { _sku         = Input Clean $ sc ^. sku
  , _description = Input Clean $ sc ^. description
  , _serial      = Input Clean $ sc ^. serial
  , _squadron    = (sc ^. squadron) `withOptions'` fullset
  , _operable    = (sc ^. operable) `withOptions'` fullset
  }


fromEditForm :: EditForm -> Maybe SpaceCraftUpdate
fromEditForm ef = do
  _sku         <- ef ^? sku . value
  _description <- ef ^? description . Form.value
  _serial      <- ef ^? serial . value
  _squadron    <- ef ^. squadron . to selected
  _operable    <- ef ^? operable . to selected
  return SpaceCraftUpdate {..}


editForm :: (CRUDSpaceCraft m, MonadJSM m) => Maybe SpaceCraftId -> EditForm -> Html m EditForm
editForm mid ef = H.form_

  [ H.label [ H.for' "sku" ] [ "SKU" ]
  , ef <+ sku . mapping (_Wrapped . rounding)
  $ Input.number [ H.name' "sku" ]

  , H.label [ H.for' "description" ] [ "Description" ]
  , ef <+ description . mapping (defaulting "")
  $ Input.text [ H.name' "description" ]

  , H.label [ H.for' "serial" ] [ "Serial Number" ]
  , ef <+ serial . mapping (_Wrapped . rounding)
  $ Input.number [ H.name' "serial" ]

  , H.label [ H.for' "squadron" ] [ "Squadron" ]
  , ef <+ squadron
  $ dropdown bootstrap defConfig

  , H.label [ H.for' "operable" ] [ "Operable" ]
  , ef <+ operable
  $ dropdown bootstrap defConfig

  , H.a
    [ H.onClick' $ case (mid, fromEditForm ef) of
      (Nothing,  Just up) -> ef <$ createSpaceCraft up
      (Just sid, Just up) -> ef <$ updateSpaceCraft sid up
      _                   -> return ef
    , H.class' "btn btn-primary"
    ] [ "Save" ]

  ]


start :: (Monad m, CRUDSpaceCraft m) => Route -> m Frontend
start = \case
  RList s     -> MList . Roster (SortCol SKUT ASC) s <$> listSpaceCraft
  REcho t     -> return $ MEcho t
  RNew        -> return $ MDetail Nothing $ emptyEditForm
  RExisting i -> do
    mcraft <- getSpaceCraft i
    return $ case mcraft of
     Just craft -> MDetail (Just i) $ toEditForm craft
     _          -> M404


tableCfg :: Table.Config m a
tableCfg = Table.Config
  [ H.class' "table table-striped table-bordered" ] [] []


fuzzy :: [SpaceCraft -> Text]
fuzzy = flip (^.) <$>
    [ sku         . to humanize
    , description . to humanize
    , serial      . to humanize
    , squadron    . to humanize
    , operable    . to humanize
    ]


view :: (MonadJSM m, CRUDSpaceCraft m) => Frontend -> Html m Frontend
view fe = case fe of
  MList r -> MList <$> H.div "container-fluid"
   [ H.div "flex justify-content-between"
     [ H.h2_ [ "Space Craft Roster" ]
     , H.div [ H.class' "input-group flex-nowrap", H.textProperty "style" ("width:200px" :: Text) ]
       [ r <+ search $ Input.search [ H.class' "form-control", H.placeholder "Search" ]
       ]
     , H.a [ H.onClick' (r <$ navigate @SPA RNew), H.class' "btn btn-primary" ] [ "Register" ]
     ]
   , r <+ sort $ Table.viewWith tableCfg $ r ^. table . to (fuzzySearch fuzzy $ r ^. search . value)
   , H.a [ H.onClick' (r <$ navigate @SPA (REcho $ Just "plex")) ] [ text "Go to Echo" ]
   ]
  MDetail sid form -> MDetail sid <$> H.div "container-fluid"
    [ H.div "d-flex justify-content-between"
      [ H.h2_ [ text $ maybe "Register New Space Craft" (\i -> "Edit Space Craft: " <> pack (show i)) sid ]
      , editForm sid form
      ]
    ]
  MEcho t -> H.div_
    [ maybe (text "Erie silence") text t
    , H.a [ H.onClick' (fe <$ navigate @SPA (RList $ Input Clean "")) ] [ "Go To Space Craft Roster" ]
    ]
  M404 -> text "404"


template :: Frontend -> Html m a -> Html m a
template fe stage = H.html_
  [ H.head_
    [ H.link'
      [ H.rel "stylesheet"
      , H.href "https://cdn.usebootstrap.com/bootstrap/4.3.1/css/bootstrap.min.css"
      ]
    , H.meta [ H.charset "ISO-8859-1" ] []
    , toHydration fe
    , H.script [ H.src "/all.js" ] []
    ]
  , H.body_
    [ stage
    ]
  ]
