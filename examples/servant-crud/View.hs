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
import           Shpadoinkle                       (Html, MonadJSM, text)
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Lens
import           Shpadoinkle.Router                (navigate, toHydration)
import           Shpadoinkle.Run                   (Env, entrypoint)
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
                                                    getValid, humanize, present,
                                                    validate, withOptions')

import           Types


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
      , H.class' ("form-control":controlClass (errs ^. l) (ef ^. l .hygiene))
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
    [ ef <% l $ Input.integral
      [ H.name' hName, H.step "1", H.min "0"
      , H.class' ("form-control":controlClass (errs ^. l) (ef ^. l .hygiene))
      ]
    ]
    <> invalid (errs ^. l) (ef ^. l . hygiene)
  ] where hName = toHtmlName msg


selectControl
  :: forall p x m a
   . MonadJSM m => Control (Dropdown p)
  => Considered p ~ Maybe => Consideration ConsideredChoice p
  => Present (Selected p x) => Ord x => Present x
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
      [ H.class' [ ("dropdown", True)
                 , ("show", _toggle == Open) ]
      ]
    , _header  = pure . H.button
      [ H.class' ([ "btn", "btn-secondary", "dropdown-toggle" ] :: [Text])
      , H.type' "button"
      ] . present
    , _list    = H.div
      [ H.class' [ ("dropdown-menu", True)
                 , ("show", _toggle == Open) ]
      ]
    , _item    = H.a [ H.className "dropdown-item"
                     , H.textProperty "style" "cursor:pointer" ] . present
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


editForm :: forall m. (CRUDSpaceCraft m, MonadJSM m) => Maybe SpaceCraftId -> SpaceCraftUpdate 'Edit -> Html m (SpaceCraftUpdate 'Edit)
editForm mid ef = H.div_

  [ intControl    @SKU                   sku         "SKU"           errs ef
  , textControl   @Description           description "Description"   errs ef
  , intControl    @SerialNumber          serial      "Serial Number" errs ef
  , selectControl @'One @Squadron        squadron    "Squadron"      errs ef
  , selectControl @'AtleastOne @Operable operable    "Operable"      errs ef
  , H.div "d-flex flex-row justify-content-end"

    [ H.button
      [ H.onClickM_ . navigate @(SPA m) $ RList mempty
      , H.class' "btn btn-secondary"
      ] [ "Cancel" ]

    , H.button
      [ H.onClickM_ $ case isValid of
         Nothing -> return ()
         Just up -> do
           case mid of Nothing  -> () <$ createSpaceCraft up
                       Just sid -> updateSpaceCraft sid up
           navigate @(SPA m) (RList mempty)
      , H.class' "btn btn-primary"
      , H.disabled $ isNothing isValid
      ] [ "Save" ]

    ]
  ] where errs = validate ef
          isValid = getValid errs


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
  { tableProps = const . const . pure $ H.class' "table table-striped table-bordered"
  , tdProps    = const . const . const $ \case
      ToolsT -> [ H.width 1 ]
      _      -> "align-middle"
  }


fuzzy :: [SpaceCraft -> Text]
fuzzy = flip (^.) <$>
  [ sku         . to humanize
  , description . to humanize
  , serial      . to humanize
  , squadron    . to humanize
  , operable    . to humanize
  ]


view :: forall m. (MonadJSM m, CRUDSpaceCraft m) => Frontend -> Html m Frontend
view fe = case fe of

  MList r -> onSum _MList $ H.div "container-fluid"
   [ H.div "row justify-content-between align-items-center"
     [ H.h2_ [ "Space Craft Roster" ]
     , H.div [ H.class' "input-group"
             , H.textProperty "style" ("width:300px" :: Text)
             ]
       [ r <% search $ Input.search [ H.class' "form-control", H.placeholder "Search" ]
       , H.div "input-group-append mr-3"
         [ H.button [ H.onClickM_ $ navigate @(SPA m) RNew, H.class' "btn btn-primary" ] [ "Register" ]
         ]
       ]
     ]
   , onRecord (lensProduct table sort) $ Table.viewWith tableCfg
       (r ^. table . to (fuzzySearch fuzzy $ r ^. search . value))
       (r ^. sort)
   ]

  MDetail sid form -> onSum (_MDetail . _2) $ H.div "row"
    [ H.div "col-sm-8 offset-sm-2"
      [ H.h2_ [ text $ maybe "Register New Space Craft" (const "Edit Space Craft") sid
              ]
      , editForm sid form
      ]
    ]

  MEcho t -> H.div_
    [ maybe (text "Erie silence") text t
    , H.a [ H.onClickM_ . navigate @(SPA m) $ RList mempty ] [ "Go To Space Craft Roster" ]
    ]

  M404 -> text "404"


template :: Env -> Frontend -> Html m a -> Html m a
template ev fe stage = H.html_
  [ H.head_
    [ H.link'
      [ H.rel "stylesheet"
      , H.href "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.3.1/css/bootstrap.min.css"
      ]
    , H.meta [ H.charset "ISO-8859-1" ] []
    , toHydration fe
    , H.script [ H.src $ entrypoint ev ] []
    ]
  , H.body_
    [ stage
    ]
  ]


