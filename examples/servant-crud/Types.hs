{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Types (module Types, module Types.Prim) where


import           Control.Lens                      as Lens (Identity,
                                                            makeFieldsNoPrefix,
                                                            makeLenses,
                                                            makePrisms, view,
                                                            (^.))
import           Control.Lens.TH                   ()
import           Control.Monad.Except              (MonadError (throwError),
                                                    MonadTrans (..))
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Function                     (on)
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
import           Shpadoinkle                       (Html, MonadJSM)
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
  { _identity    :: Columnar f SpaceCraftId
  , _sku         :: Columnar f SKU
  , _description :: Columnar (Nullable f) Description
  , _serial      :: Columnar f SerialNumber
  , _squadron    :: Columnar f Squadron
  , _operable    :: Columnar f Operable
  } deriving (Generic, Beamable)


instance Table SpaceCraftT where
  newtype PrimaryKey SpaceCraftT f = SpaceCraftKey (Columnar f SpaceCraftId) deriving (Generic) deriving anyclass (Beamable)
  primaryKey = SpaceCraftKey . _identity


type SpaceCraft = SpaceCraftT Identity
deriving instance Eq SpaceCraft
deriving instance Ord SpaceCraft
deriving instance Show SpaceCraft
deriving instance ToJSON SpaceCraft
deriving instance FromJSON SpaceCraft


makeFieldsNoPrefix ''SpaceCraftT


newtype DB f = DB { _roster :: f (TableEntity SpaceCraftT) } deriving (Generic) deriving anyclass (Database be)


db :: DatabaseSettings be DB
db = defaultDbSettings


data SpaceCraftUpdate s = SpaceCraftUpdate
  { _sku         :: Field s Text Input SKU
  , _description :: Field s Text Input (Maybe Description)
  , _serial      :: Field s Text Input SerialNumber
  , _squadron    :: Field s Text (Dropdown 'One) Squadron
  , _operable    :: Field s Text (Dropdown 'AtleastOne) Operable
  } deriving Generic


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
    { _sku         = positive <> nonZero
    , _description = nonMEmpty
    , _serial      = between (30, maxBound)
    , _squadron    = maybe (throwError "Cannot be empty") pure
    , _operable    = pure
    }


makeFieldsNoPrefix ''SpaceCraftUpdate


data Roster = Roster
  { _sort   :: SortCol [SpaceCraft]
  , _search :: Input Search
  , _table  :: [SpaceCraft]
  }


deriving instance Eq      Roster
deriving instance Ord     Roster
deriving instance Show    Roster
deriving instance Generic Roster
instance (ToJSON   (Table.Column [SpaceCraft])) => ToJSON   Roster
instance (FromJSON (Table.Column [SpaceCraft])) => FromJSON Roster


makeFieldsNoPrefix ''Roster


emptyEditForm :: SpaceCraftUpdate 'Edit
emptyEditForm = SpaceCraftUpdate
  { _sku         = Input Clean 0
  , _description = Input Clean Nothing
  , _serial      = Input Clean 0
  , _squadron    = fullOptions
  , _operable    = fullOptionsMin
  }


data Frontend
  = MEcho (Maybe Text)
  | MList Roster
  | MDetail (Maybe SpaceCraftId) (SpaceCraftUpdate 'Edit)
  | M404
  deriving (Eq, Ord, Show, Generic)


instance (ToJSON   (Column [SpaceCraft])) => ToJSON   Frontend
instance (FromJSON (Column [SpaceCraft])) => FromJSON Frontend


makePrisms ''Frontend


data Route
  = REcho (Maybe Text)
  | RList (Input Search)
  | RNew
  | RExisting SpaceCraftId
  deriving (Eq, Ord, Show, Generic)


makeLenses ''Route


type API = "api" :> "space-craft" :> Get '[JSON] [SpaceCraft]
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> Get '[JSON] (Maybe SpaceCraft)
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> ReqBody '[JSON] (SpaceCraftUpdate 'Valid) :> Post '[JSON] ()
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] (SpaceCraftUpdate 'Valid) :> Put '[JSON] SpaceCraftId
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] SpaceCraftId :> Delete '[JSON] ()


type SPA m = "app" :> "echo" :> QueryParam "echo" Text :> View m Text
        :<|> "app" :> "new"  :> View m Frontend
        :<|> "app" :> "edit" :> Capture "id" SpaceCraftId :> View m Frontend
        :<|> "app" :> QueryParam "search" Search :> View m Frontend
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
    RNew        -> Redirect (Proxy @("app" :> "new" :> View m Frontend)) id
    RExisting i -> Redirect (Proxy @("app" :> "edit" :> Capture "id" SpaceCraftId :> View m Frontend)) ($ i)
    RList s     -> Redirect (Proxy @("app" :> QueryParam "search" Search :> View m Frontend)) ($ Just (_value s))


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
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)


newtype instance Row [SpaceCraft] = SpaceCraftRow { unRow :: SpaceCraft }
    deriving (Eq, Ord, Show)


instance Tabular [SpaceCraft] where

  type Effect [SpaceCraft] m = (MonadJSM m, CRUDSpaceCraft m)

  toRows = fmap SpaceCraftRow

  toCell :: forall m. Effect [SpaceCraft] m => [SpaceCraft] -> Row [SpaceCraft] -> Column [SpaceCraft] -> [Html m [SpaceCraft]]
  toCell _ (SpaceCraftRow SpaceCraft {..}) = \case
    SKUT          -> present _sku
    DescriptionT  -> present _description
    SerialNumberT -> present _serial
    SquadronT     -> present _squadron
    OperableT     -> present _operable
    ToolsT        ->
      [ H.div "btn-group"
        [ H.button [ H.className "btn btn-sm btn-secondary",
                     H.onClickM_ $ navigate @ (SPA m) (RExisting _identity) ] [ "Edit" ]
        , H.button [ H.className "btn btn-sm btn-secondary",
                     H.onClickM $ do
                       deleteSpaceCraft _identity
                       return . Prelude.filter $ \x -> x ^. identity /= _identity ] [ "Delete" ]
        ]
      ]

  sortTable (SortCol c d) = f $ case c of
    SKUT          -> g sku
    DescriptionT  -> g description
    SerialNumberT -> g serial
    SquadronT     -> g squadron
    OperableT     -> g operable
    ToolsT        -> \_ _ -> EQ
    where f = case d of ASC -> id; DESC -> flip
          g l = compare `on` Lens.view l . unRow
