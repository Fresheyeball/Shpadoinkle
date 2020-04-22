{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module View where


import           Control.Lens                      hiding (view)
import           Control.Lens.Unsound              (lensProduct)
import           Data.Coerce                       (Coercible)
import           Data.Maybe                        (fromMaybe, isNothing)
import           Data.String                       (IsString)
import           Data.Text                         as T
import           Prelude                           hiding (div)
import           Shpadoinkle                       (Html, MonadJSM, text)
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Lens                  ((<%), (<+))
import           Shpadoinkle.Router                (navigate, toHydration)
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown (Dropdown (..),
                                                                Theme (..),
                                                                defConfig,
                                                                dropdown)
import qualified Shpadoinkle.Widgets.Form.Input    as Input
import           Shpadoinkle.Widgets.Table         as Table
import           Shpadoinkle.Widgets.Types         (Consideration, Considered,
                                                    ConsideredChoice,
                                                    Control (..), Field,
                                                    Hygiene (..), Input (..),
                                                    Pick (..), Present,
                                                    Selected, Status (..),
                                                    Toggle (..), Validated (..),
                                                    fullset, fuzzySearch,
                                                    getValid, humanize,
                                                    validate, withOptions')

import           Types

import           Debug.Trace


default (Text, [])


toEditForm :: SpaceCraft -> SpaceCraftUpdate 'Edit
toEditForm sc = SpaceCraftUpdate
  { _sku         = pure $ sc ^. sku
  , _description = pure $ sc ^. description
  , _serial      = pure $ sc ^. serial
  , _squadron    = (sc ^. squadron) `withOptions'` fullset
  , _operable    = (sc ^. operable) `withOptions'` fullset
  }


formGroup :: [Html m a] -> Html m a
formGroup = H.div "form-group row"


textControl
  :: forall t m a
   . Eq t => IsString t => Coercible Text t => MonadJSM m
  => (forall v. Lens' (a v) (Field v Text Input (Maybe t)))
  -> Text -> a 'Errors -> a 'Edit -> Html m (a 'Edit)
textControl l msg errs ef = formGroup
  [ H.label [ H.for' hName, H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" $
    [ ef <% l . mapping (fromMaybe "" `iso` noEmpty) $ Input.text
      [ H.name' hName
      , H.className ("form-control":controlClass (errs ^. l) (ef ^. l .hygiene))
      ]
    ]
    <> invalid (errs ^. l) (ef ^. l . hygiene)
  ] where hName = toHtmlName msg
          noEmpty "" = Nothing
          noEmpty x  = Just x


intControl
  :: forall n m a
   . MonadJSM m => Integral n => Show n
  => (forall v. Lens' (a v) (Field v Text Input n))
  -> Text -> a 'Errors -> a 'Edit -> Html m (a 'Edit)
intControl l msg errs ef = formGroup
  [ H.label [ H.for' hName, H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" $
    [ ef <% l $ Input.integral @m
      $ [ H.name' hName, H.step "1", H.min "0"
        , H.className ("form-control":controlClass (errs ^. l) (ef ^. l .hygiene))
        ]
    ]
    <> invalid (errs ^. l) (ef ^. l . hygiene)
  ] where hName = toHtmlName msg


selectControl
  :: forall p x m a
   . MonadJSM m => Control (Dropdown p)
  => Considered p ~ Maybe => Consideration ConsideredChoice p
  => Present (Selected p x) => Present x => Ord x
  => (forall v. Lens' (a v) (Field v Text (Dropdown p) x))
  -> Text -> a 'Errors -> a 'Edit -> Html m (a 'Edit)
selectControl l msg errs ef = formGroup
  [ H.label [ H.for' (toHtmlName msg)
            , H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" $
    [ ef <% l $ dropdown bootstrap defConfig ]
    <> invalid (errs ^. l) (ef ^. l . hygiene)
  ]
  where
  bootstrap Dropdown {..} = Dropdown.Theme
    { _wrapper = H.div
      [ H.className [ ("dropdown", True)
                    , ("show", _toggle == Open) ]
      ]
    , _header  = pure . H.button
      [ H.className ([ "btn", "btn-secondary", "dropdown-toggle" ] :: [Text])
      , H.type' "button"
      ]
    , _list    = H.div
      [ H.className [ ("dropdown-menu", True)
                    , ("show", _toggle == Open) ]
      ]
    , _item    = H.a [ H.className "dropdown-item"
                     , H.textProperty "style" "cursor:pointer" ]
    }


controlClass :: Validated e a -> Hygiene -> [Text]
controlClass (Invalid _ _) Dirty = ["is-invalid"]
controlClass (Validated _) Dirty = ["is-valid"]
controlClass _ Clean             = []


invalid :: Validated Text a -> Hygiene -> [ Html m b ]
invalid (Invalid err errs) Dirty = (\e -> H.div "invalid-feedback" [ text e ]) <$> err:errs
invalid _                  _     = []


toHtmlName :: Text -> Text
toHtmlName = toLower . replace " " "-"


editForm :: (CRUDSpaceCraft m, MonadJSM m) => Maybe SpaceCraftId -> SpaceCraftUpdate 'Edit -> Html m (SpaceCraftUpdate 'Edit)
editForm mid ef = H.div_

  [ intControl    @SKU                   sku         "SKU"           errs ef
  , textControl   @Description           description "Description"   errs ef
  , intControl    @SerialNumber          serial      "Serial Number" errs ef
  , selectControl @'One @Squadron        squadron    "Squadron"      errs ef
  , selectControl @'AtleastOne @Operable operable    "Operable"      errs ef
  , H.div "d-flex flex-row justify-content-end"

    [ H.button
      [ H.onClick' (ef <$ navigate @SPA (RList mempty))
      , H.class' "btn btn-secondary"
      ] [ "Cancel" ]

    , H.button
      [ H.onClick' $ case isValid of
         Nothing -> return ef
         Just up -> do
           case mid of Nothing  -> () <$ createSpaceCraft up
                       Just sid -> updateSpaceCraft sid up
           ef <$ navigate @SPA (RList mempty)
      , H.class' "btn btn-primary"
      , H.disabled $ isNothing isValid
      ] [ "Save" ]
    ]
  ] where errs = trace (show $ validate ef) $ validate $ trace (show ef) ef
          isValid = trace (show $ getValid errs) $ getValid errs


start :: (Monad m, CRUDSpaceCraft m) => Route -> m Frontend
start = \case
  RList s     -> MList . Roster (SortCol SKUT ASC) s <$> listSpaceCraft
  REcho t     -> return $ MEcho t
  RNew        -> return $ MDetail Nothing emptyEditForm
  RExisting i -> do
    mcraft <- getSpaceCraft i
    return $ case mcraft of
     Just craft -> MDetail (Just i) $ toEditForm craft
     _          -> M404


tableCfg :: Table.Theme m [SpaceCraft]
tableCfg = mempty
  { tableProps = [ H.class' "table table-striped table-bordered" ]
  , tdProps    = \case
      ToolsT -> [ H.width 1 ]
      _ -> "align-middle"
  }


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
   [ H.div "row justify-content-between align-items-center"
     [ H.h2_ [ "Space Craft Roster" ]
     , H.div [ H.class' "input-group"
             , H.textProperty "style" ("width:300px" :: Text)
             ]
       [ r <% search $ Input.search [ H.class' "form-control", H.placeholder "Search" ]
       , H.div "input-group-append mr-3"
         [ H.button [ H.onClick' (r <$ navigate @SPA RNew), H.class' "btn btn-primary" ] [ "Register" ]
         ]
       ]
     ]
   , r <+ lensProduct table sort $ Table.viewWith tableCfg
    (r ^. table . to (fuzzySearch fuzzy $ r ^. search . value))
    (r ^. sort)
   ]

  MDetail sid form -> MDetail sid <$> H.div "row"
    [ H.div "col-sm-8 offset-sm-2"
      [ H.h2_ [ text $ maybe "Register New Space Craft" (const "Edit Space Craft") sid
              ]
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
