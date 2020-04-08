{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module View where


import           Control.Lens                      hiding (view)
import           Control.Lens.Unsound              (lensProduct)
import           Data.Coerce
import           Data.String
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


default (Text)


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


formGroup :: [Html m a] -> Html m a
formGroup = H.div "form-group row"


textControl
  :: IsString t => Coercible Text t => MonadJSM m
  => Lens' a (Input (Maybe t)) -> Text -> a -> Html m a
textControl l msg ef = formGroup
  [ H.label [ H.for' hName, H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" [ ef <% l . mapping (defaulting "") $ Input.text [ H.name' hName, H.class' "form-control" ] ]
  ] where hName = toHtmlName msg


intControl
  :: forall m n a
   . MonadJSM m => Integral n
  => Lens' a (Input n) -> Text -> a -> Html m a
intControl l msg ef = formGroup
  [ H.label [ H.for' hName, H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" [ ef <% l . mapping rounding $ Input.number @m @Double [ H.name' hName, H.step "1", H.min "0", H.class' "form-control" ] ]
  ] where hName = toHtmlName msg


selectControl
  :: MonadJSM m
  => Considered p ~ Maybe => Consideration ConsideredChoice p
  => Present (Selected p x) => Present x => Ord x
  => Lens' a (Dropdown p x) -> Text -> a -> Html m a
selectControl l msg ef = formGroup
  [ H.label [ H.for' (toHtmlName msg), H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" [ ef <% l $ dropdown bootstrap defConfig ]
  ]


toHtmlName :: Text -> Text
toHtmlName = toLower . replace " " "-"


editForm :: (CRUDSpaceCraft m, MonadJSM m) => Maybe SpaceCraftId -> EditForm -> Html m EditForm
editForm mid ef = H.div_

  [ intControl    sku         "SKU"           ef
  , textControl   description "Description"   ef
  , intControl    serial      "Serial Number" ef
  , selectControl squadron    "Squadron"      ef
  , selectControl operable    "Operable"      ef

  , H.button
    [ H.onClick' $ case fromEditForm ef of
       Nothing -> return ef
       Just up -> do
         case mid of Nothing  -> () <$ createSpaceCraft up
                     Just sid -> updateSpaceCraft sid up
         ef <$ navigate @SPA (RList mempty)
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
   [ H.div "row justify-content-between"
     [ H.h2_ [ "Space Craft Roster" ]
     , H.div [ H.class' "input-group flex-nowrap"
             , H.textProperty "style" ("width:200px" :: Text)
             ]
       [ r <% search $ Input.search [ H.class' "form-control", H.placeholder "Search" ]
       , H.a [ H.onClick' (r <$ navigate @SPA RNew), H.class' "btn btn-primary" ] [ "Register" ]
       ]
     ]
   , r <+ lensProduct table sort $ Table.viewWith tableCfg (r ^. table . to (fuzzySearch fuzzy $ r ^. search . value)) (r ^. sort)
   , H.a [ H.onClick' (r <$ navigate @SPA (REcho $ Just "plex")) ] [ text "Go to Echo" ]
   ]

  MDetail sid form -> MDetail sid <$> H.div "row"
    [ H.div "col-sm-8 offset-sm-2"
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
