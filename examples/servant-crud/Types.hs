{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Types (module Types, module Types.Prim) where


import           Control.Lens                      as Lens (Identity, view,
                                                            (^.))
import           Control.Monad.Except              (MonadError (throwError),
                                                    MonadTrans (..))
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Function                     (on)
import           Data.Generics.Labels              ()
import           Data.Maybe                        (fromMaybe)
import           Data.Proxy                        (Proxy (Proxy))
import           Data.Text                         (Text)
import           Database.Beam                     (Beamable, Columnar,
                                                    Database, DatabaseSettings,
                                                    Generic, Nullable,
                                                    Table (..), TableEntity,
                                                    defaultDbSettings)

import           Servant.API                       (Capture, Delete,
                                                    FromHttpApiData, Get, JSON,
                                                    Post, Put, QueryParam, Raw,
                                                    ReqBody, ToHttpApiData,
                                                    type (:<|>) (..), type (:>))
import           Shpadoinkle                       (Html, MonadJSM, NFData)
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Router                (HasRouter (type (:>>)),
                                                    Redirect (Redirect),
                                                    Routed (..), View, navigate)
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown (Dropdown)
import           Shpadoinkle.Widgets.Table         as Table (Column, Row,
                                                             Sort (ASC, DESC),
                                                             SortCol (..),
                                                             Tabular (Effect, sortTable, toCell, toRows))
import           Shpadoinkle.Widgets.Types         (Field, Humanize (..),
                                                    Hygiene (Clean),
                                                    Input (Input, _value),
                                                    Pick (AtleastOne, One),
                                                    Present (present),
                                                    Search (Search),
                                                    Status (Edit, Errors, Valid),
                                                    Validate (rules),
                                                    fullOptions, fullOptionsMin)
import           Shpadoinkle.Widgets.Validation    (between, nonMEmpty, nonZero,
                                                    positive)

import           Types.Prim                        (Description (..),
                                                    Operable (..), SKU (..),
                                                    SerialNumber (..),
                                                    SpaceCraftId (..),
                                                    Squadron (..))


data SpaceCraftT f = SpaceCraft
  { identity    :: Columnar f SpaceCraftId
  , sku         :: Columnar f SKU
  , description :: Columnar (Nullable f) Description
  , serial      :: Columnar f SerialNumber
  , squadron    :: Columnar f Squadron
  , operable    :: Columnar f Operable
  } deriving (Generic, Beamable)


instance NFData (SpaceCraftT Identity)


instance Table SpaceCraftT where
  newtype PrimaryKey SpaceCraftT f = SpaceCraftKey (Columnar f SpaceCraftId) deriving (Generic) deriving anyclass (Beamable)
  primaryKey = SpaceCraftKey . identity


type SpaceCraft = SpaceCraftT Identity
deriving instance Eq SpaceCraft
deriving instance Ord SpaceCraft
deriving instance Show SpaceCraft
deriving instance ToJSON SpaceCraft
deriving instance FromJSON SpaceCraft


newtype DB f = DB { roster :: f (TableEntity SpaceCraftT) } deriving (Generic) deriving anyclass (Database be)


db :: DatabaseSettings be DB
db = defaultDbSettings


data SpaceCraftUpdate s = SpaceCraftUpdate
  { sku         :: Field s Text Input SKU
  , description :: Field s Text Input (Maybe Description)
  , serial      :: Field s Text Input SerialNumber
  , squadron    :: Field s Text (Dropdown 'One) Squadron
  , operable    :: Field s Text (Dropdown 'AtleastOne) Operable
  } deriving Generic


instance ( NFData (Field s Text Input SKU)
         , NFData (Field s Text Input (Maybe Description))
         , NFData (Field s Text Input SerialNumber)
         , NFData (Field s Text (Dropdown 'One) Squadron)
         , NFData (Field s Text (Dropdown 'AtleastOne) Operable)
         ) => NFData (SpaceCraftUpdate s)


deriving instance Eq       (SpaceCraftUpdate 'Valid)
deriving instance Ord      (SpaceCraftUpdate 'Valid)
deriving instance Show     (SpaceCraftUpdate 'Valid)
deriving instance ToJSON   (SpaceCraftUpdate 'Valid)
deriving instance FromJSON (SpaceCraftUpdate 'Valid)

deriving instance Eq       (SpaceCraftUpdate 'Edit)
deriving instance Ord      (SpaceCraftUpdate 'Edit)
deriving instance Show     (SpaceCraftUpdate 'Edit)
deriving instance ToJSON   (SpaceCraftUpdate 'Edit)
deriving instance FromJSON (SpaceCraftUpdate 'Edit)

deriving instance Show     (SpaceCraftUpdate 'Errors)


instance Validate SpaceCraftUpdate where
  rules = SpaceCraftUpdate
    { sku         = positive <> nonZero
    , description = nonMEmpty
    , serial      = between (30, maxBound)
    , squadron    = maybe (throwError "Cannot be empty") pure
    , operable    = pure
    }


data Roster = Roster
  { sort   :: SortCol [SpaceCraft]
  , search :: Input Search
  , table  :: [SpaceCraft]
  }


deriving instance Eq      Roster
deriving instance Ord     Roster
deriving instance Show    Roster
deriving instance Generic Roster
instance
  ( NFData (Column [SpaceCraft])
  , NFData (SpaceCraftT Identity)) => NFData Roster
instance (ToJSON   (Table.Column [SpaceCraft])) => ToJSON   Roster
instance (FromJSON (Table.Column [SpaceCraft])) => FromJSON Roster


emptyEditForm :: SpaceCraftUpdate 'Edit
emptyEditForm = SpaceCraftUpdate
  { sku         = Input Clean 0
  , description = Input Clean Nothing
  , serial      = Input Clean 0
  , squadron    = fullOptions
  , operable    = fullOptionsMin
  }


data ViewModel
  = MEcho (Maybe Text)
  | MList Roster
  | MDetail (Maybe SpaceCraftId) (SpaceCraftUpdate 'Edit)
  | M404
  deriving (Eq, Ord, Show, Generic)


instance (ToJSON   (Column [SpaceCraft])) => ToJSON   ViewModel
instance (FromJSON (Column [SpaceCraft])) => FromJSON ViewModel
instance (NFData   (Column [SpaceCraft])) => NFData   ViewModel


data Route
  = REcho (Maybe Text)
  | RList (Input Search)
  | RNew
  | RExisting SpaceCraftId
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)


type API = "api" :> "space-craft" :> Get '[JSON] [SpaceCraft]
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> Get '[JSON] (Maybe SpaceCraft)
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> ReqBody '[JSON] (SpaceCraftUpdate 'Valid) :> Post '[JSON] ()
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] (SpaceCraftUpdate 'Valid) :> Put '[JSON] SpaceCraftId
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] SpaceCraftId :> Delete '[JSON] ()


type SPA m = "app" :> "echo" :> QueryParam "echo" Text :> View m Text
        :<|> "app" :> "new"  :> View m ViewModel
        :<|> "app" :> "edit" :> Capture "id" SpaceCraftId :> View m ViewModel
        :<|> "app" :> QueryParam "search" Search :> View m ViewModel
        :<|> Raw


routes :: SPA m :>> Route
routes = REcho
    :<|> RNew
    :<|> RExisting
    :<|> RList . Input Clean . fromMaybe ""
    :<|> RList (Input Clean "")


deriving newtype instance ToHttpApiData   Search
deriving newtype instance FromHttpApiData Search


instance Routed (SPA m) Route where
  redirect = \case
    REcho t     -> Redirect (Proxy @("app" :> "echo" :> QueryParam "echo" Text :> View m Text)) ($ t)
    RNew        -> Redirect (Proxy @("app" :> "new" :> View m ViewModel)) id
    RExisting i -> Redirect (Proxy @("app" :> "edit" :> Capture "id" SpaceCraftId :> View m ViewModel)) ($ i)
    RList s     -> Redirect (Proxy @("app" :> QueryParam "search" Search :> View m ViewModel)) ($ Just (_value s))


class CRUDSpaceCraft m where
  listSpaceCraft   :: m [SpaceCraft]
  getSpaceCraft    :: SpaceCraftId -> m (Maybe SpaceCraft)
  updateSpaceCraft :: SpaceCraftId -> SpaceCraftUpdate 'Valid -> m ()
  createSpaceCraft :: SpaceCraftUpdate 'Valid -> m SpaceCraftId
  deleteSpaceCraft :: SpaceCraftId -> m ()


instance (MonadTrans t, Monad m, CRUDSpaceCraft m) => CRUDSpaceCraft (t m) where
  listSpaceCraft     = lift listSpaceCraft
  getSpaceCraft      = lift . getSpaceCraft
  updateSpaceCraft x = lift . updateSpaceCraft x
  createSpaceCraft   = lift . createSpaceCraft
  deleteSpaceCraft   = lift . deleteSpaceCraft


instance Humanize (Column [SpaceCraft]) where
  humanize = \case
    SKUT          -> "SKU"
    DescriptionT  -> "Desc"
    SerialNumberT -> "Serial #"
    SquadronT     -> "Squadron"
    OperableT     -> "Status"
    ToolsT        -> ""


data instance Column [SpaceCraft] =
    SKUT | DescriptionT | SerialNumberT | SquadronT | OperableT | ToolsT
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON, NFData)


newtype instance Row [SpaceCraft] = SpaceCraftRow { unRow :: SpaceCraft }
    deriving (Eq, Ord, Show)


instance Tabular [SpaceCraft] where

  type Effect [SpaceCraft] m = (MonadJSM m, CRUDSpaceCraft m)

  toRows = fmap SpaceCraftRow

  toCell :: forall m. Effect [SpaceCraft] m => [SpaceCraft] -> Row [SpaceCraft] -> Column [SpaceCraft] -> [Html m [SpaceCraft]]
  toCell _ (SpaceCraftRow SpaceCraft {..}) = \case
    SKUT          -> present sku
    DescriptionT  -> present description
    SerialNumberT -> present serial
    SquadronT     -> present squadron
    OperableT     -> present operable
    ToolsT        ->
      [ H.div "btn-group"
        [ H.button [ H.className "btn btn-sm btn-secondary",
                     H.onClickM_ $ navigate @ (SPA m) (RExisting identity) ] [ "Edit" ]
        , H.button [ H.className "btn btn-sm btn-secondary",
                     H.onClickM $ do
                       deleteSpaceCraft identity
                       return . Prelude.filter $ \x -> x ^. #identity /= identity ] [ "Delete" ]
        ]
      ]

  sortTable (SortCol c d) = f $ case c of
    SKUT          -> g #sku
    DescriptionT  -> g #description
    SerialNumberT -> g #serial
    SquadronT     -> g #squadron
    OperableT     -> g #operable
    ToolsT        -> \_ _ -> EQ
    where f = case d of ASC -> id; DESC -> flip
          g l = compare `on` Lens.view l . unRow
