{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Types where


import           Control.Lens                      as Lens
import           Control.Lens.TH                   ()
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Function
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           Data.Text
import           Database.Beam
#ifndef ghcjs_HOST_OS
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Syntax
import           Database.SQLite.Simple.FromField
#endif
import           Servant.API                       hiding (Description)
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Router
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown
import           Shpadoinkle.Widgets.Table         as Table
import           Shpadoinkle.Widgets.Types


newtype SKU = SKU { unSKU :: Int  }
  deriving stock Generic
  deriving newtype (Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present)
#ifndef ghcjs_HOST_OS
  deriving newtype (FromBackendRow Sqlite)
#endif
instance Wrapped SKU
instance Rewrapped SKU SKU


newtype Description = Description { unDescription  :: Text }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Read, IsString, ToJSON, FromJSON, Humanize, Semigroup, Monoid)
  deriving anyclass (Present)
#ifndef ghcjs_HOST_OS
  deriving newtype (FromBackendRow Sqlite)
#endif

instance Humanize (Maybe Description) where
  humanize = maybe "N/A" humanize

newtype SerialNumber = SerialNumber { unSerialNumber :: Integer  }
  deriving stock Generic
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present)
#ifndef ghcjs_HOST_OS
  deriving newtype (FromBackendRow Sqlite)
#endif
instance Wrapped SerialNumber
instance Rewrapped SerialNumber SerialNumber

newtype SpaceCraftId = SpaceCraftId { unSpaceCraftId :: Int }
  deriving newtype ( Eq, Ord, Show, Num, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)
  deriving anyclass (Humanize, Present)
#ifndef ghcjs_HOST_OS
  deriving newtype (FromBackendRow Sqlite, HasSqlValueSyntax SqliteValueSyntax, HasSqlEqualityCheck Sqlite)
#endif


data Operable = Inoperable | Operational
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Humanize, Present, Generic, ToJSON, FromJSON)
#ifndef ghcjs_HOST_OS
  deriving (FromBackendRow Sqlite)
#endif


data Squadron = AwayTeam | StrikeForce | Scout
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Present, Generic, ToJSON, FromJSON)
#ifndef ghcjs_HOST_OS
  deriving (FromBackendRow Sqlite)
#endif


#ifndef ghcjs_HOST_OS
instance FromField Operable where fromField = fmap read <$> fromField
instance FromField Squadron where fromField = fmap read <$> fromField
#endif


instance Humanize Squadron where
  humanize = \case
    AwayTeam    -> "Away Team"
    StrikeForce -> "Strike Force"
    Scout       -> "Scouting"


instance Humanize (Maybe Squadron) where
  humanize = maybe "N/A" humanize


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


data DB f = DB { _roster :: f (TableEntity SpaceCraftT) } deriving (Generic, Database be)


db :: DatabaseSettings be DB
db = defaultDbSettings


data SpaceCraftUpdate = SpaceCraftUpdate
  { _sku         :: SKU
  , _description :: Description
  , _serial      :: SerialNumber
  , _squadron    :: Squadron
  , _operable    :: Operable
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


makeFieldsNoPrefix ''SpaceCraftUpdate


data Roster = Roster
  { _sort   :: SortCol [SpaceCraft]
  , _search :: Input Search
  , _table  :: [SpaceCraft]
  }


deriving instance Eq   Roster
deriving instance Ord  Roster
deriving instance Show Roster
deriving instance Generic Roster
instance (ToJSON   (TableColumn [SpaceCraft])) => ToJSON   Roster
instance (FromJSON (TableColumn [SpaceCraft])) => FromJSON Roster


makeFieldsNoPrefix ''Roster


data EditForm = EditForm
  { _sku         :: Input SKU
  , _description :: Input (Maybe Description)
  , _serial      :: Input SerialNumber
  , _squadron    :: Dropdown 'One Squadron
  , _operable    :: Dropdown 'AtleastOne Operable
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


makeFieldsNoPrefix ''EditForm


emptyEditForm :: EditForm
emptyEditForm = EditForm
  { _sku         = Input Clean 0
  , _description = Input Clean Nothing
  , _serial      = Input Clean 0
  , _squadron    = fullOptions
  , _operable    = fullOptionsMin
  }


data Frontend
  = MEcho (Maybe Text)
  | MList Roster
  | MDetail (Maybe SpaceCraftId) EditForm
  | M404
  deriving (Eq, Ord, Show, Generic)


instance (ToJSON   (TableColumn [SpaceCraft])) => ToJSON   Frontend
instance (FromJSON (TableColumn [SpaceCraft])) => FromJSON Frontend


makeLenses ''Frontend


data Route
  = REcho (Maybe Text)
  | RList (Input Search)
  | RNew
  | RExisting SpaceCraftId
  deriving (Eq, Ord, Show, Generic)


makeLenses ''Route


type API = "api" :> "space-craft" :> Get '[JSON] [SpaceCraft]
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> Get '[JSON] (Maybe SpaceCraft)
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> ReqBody '[JSON] SpaceCraftUpdate :> Post '[JSON] ()
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] SpaceCraftUpdate :> Put '[JSON] SpaceCraftId
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] SpaceCraftId :> Delete '[JSON] ()


type SPA = "app" :> "echo" :> QueryParam "echo" Text :> Raw
      :<|> "app" :> "new"  :> Raw
      :<|> "app" :> "edit" :> Capture "id" SpaceCraftId :> Raw
      :<|> "app" :> QueryParam "search" Search :> Raw
      :<|> Raw


routes :: SPA :>> Route
routes = REcho
    :<|> RNew
    :<|> RExisting
    :<|> RList . Input Clean . fromMaybe ""
    :<|> RList (Input Clean "")


deriving newtype instance ToHttpApiData   Search
deriving newtype instance FromHttpApiData Search


instance Routed SPA Route where
  redirect = \case
    REcho t     -> Redirect (Proxy @("app" :> "echo" :> QueryParam "echo" Text :> Raw)) ($ t)
    RNew        -> Redirect (Proxy @("app" :> "new" :> Raw)) id
    RExisting i -> Redirect (Proxy @("app" :> "edit" :> Capture "id" SpaceCraftId :> Raw)) ($ i)
    RList s     -> Redirect (Proxy @("app" :> QueryParam "search" Search :> Raw)) ($ Just (_value s))


class CRUDSpaceCraft m where
  listSpaceCraft   :: m [SpaceCraft]
  getSpaceCraft    :: SpaceCraftId -> m (Maybe SpaceCraft)
  updateSpaceCraft :: SpaceCraftId -> SpaceCraftUpdate -> m ()
  createSpaceCraft :: SpaceCraftUpdate -> m SpaceCraftId
  deleteSpaceCraft :: SpaceCraftId -> m ()


instance (MonadTrans t, Monad m, CRUDSpaceCraft m) => CRUDSpaceCraft (t m) where
  listSpaceCraft     = lift listSpaceCraft
  getSpaceCraft      = lift . getSpaceCraft
  updateSpaceCraft x = lift . updateSpaceCraft x
  createSpaceCraft   = lift . createSpaceCraft
  deleteSpaceCraft   = lift . deleteSpaceCraft


instance Humanize (TableColumn [SpaceCraft]) where
  humanize = \case
    SKUT          -> "SKU"
    DescriptionT  -> "Desc"
    SerialNumberT -> "Serial #"
    SquadronT     -> "Squadron"
    OperableT     -> "Operable"
    ToolsT        -> ""


instance TableTerritory [SpaceCraft] where
  data TableColumn [SpaceCraft] =
    SKUT | DescriptionT | SerialNumberT | SquadronT | OperableT | ToolsT
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)

  newtype TableRow [SpaceCraft] = Row { unRow :: SpaceCraft }
    deriving (Eq, Ord, Show)

  toRows = fmap Row

  toCell (Row SpaceCraft {..}) = \case
    SKUT          -> present _sku
    DescriptionT  -> present _description
    SerialNumberT -> present _serial
    SquadronT     -> present _squadron
    OperableT     -> present _operable
    ToolsT        ->
      [ H.a [ H.onClick' (navigate @ SPA (RExisting _identity)) ] [ "Edit" ]
      ]

  sortTable (SortCol c d) = f $ case c of
    SKUT          -> compare `on` Lens.view sku         . unRow
    DescriptionT  -> compare `on` Lens.view description . unRow
    SerialNumberT -> compare `on` Lens.view serial      . unRow
    SquadronT     -> compare `on` Lens.view squadron    . unRow
    OperableT     -> compare `on` Lens.view operable    . unRow
    ToolsT        -> \_ _ -> EQ
    where f = case d of ASC -> id; DESC -> flip


