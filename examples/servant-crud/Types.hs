{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
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
import           Data.Aeson
import           Data.Function
import           Data.Maybe
import           Data.Proxy
import           Data.Set                          as Set
import           Data.String
import           Data.Text
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Syntax
import           Database.SQLite.Simple.FromField
import           Servant.API                       hiding (Description)
import           Shpadoinkle
import           Shpadoinkle.Router
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown
import           Shpadoinkle.Widgets.Table         as Table
import           Shpadoinkle.Widgets.Types


newtype SKU = SKU { unSKU :: Int  }
  deriving newtype (Eq, Ord, Show, Read, Num, ToJSON, FromJSON, FromBackendRow Sqlite)
  deriving anyclass (Humanize, Present)


newtype Description = Description { unDescription  :: Text }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Read, IsString, ToJSON, FromJSON, FromBackendRow Sqlite)
  deriving anyclass (Humanize, Present)


newtype SerialNumber = SerialNumber { unSerialNumber :: Int  }
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON, FromBackendRow Sqlite)
  deriving anyclass (Humanize, Present)


newtype SpaceCraftId = SpaceCraftId { unSpaceCraftId :: Int }
  deriving newtype ( Eq, Ord, Show, Num, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData
                   , FromBackendRow Sqlite, HasSqlValueSyntax SqliteValueSyntax, HasSqlEqualityCheck Sqlite)
  deriving anyclass (Humanize, Present)


data Operable = Inoperable | Operational
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Humanize, Present, Generic, ToJSON, FromJSON, FromBackendRow Sqlite)


instance FromField Operable where fromField = fmap read <$> fromField


data Squadron = AwayTeam | StrikeForce | Scout
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Present, Generic, ToJSON, FromJSON, FromBackendRow Sqlite)


instance FromField Squadron where fromField = fmap read <$> fromField


instance Humanize Squadron where
  humanize = \case
    AwayTeam    -> "Away Team"
    StrikeForce -> "Strike Force"
    Scout       -> "Scouting"


data SpaceCraftT f = SpaceCraft
  { _identity    :: Columnar f SpaceCraftId
  , _sku         :: Columnar f SKU
  , _description :: Columnar f (Maybe Description)
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
  { _sku         :: Maybe SKU
  , _description :: Maybe Description
  , _serial      :: Maybe SerialNumber
  , _squadron    :: Maybe Squadron
  , _operable    :: Maybe Operable
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


makeFieldsNoPrefix ''SpaceCraftUpdate


data Roster = Roster
  { _sort   :: SortCol (Set SpaceCraft)
  , _search :: Input Search
  , _table  :: Set SpaceCraft
  }


makeFieldsNoPrefix ''Roster


data EditForm = EditForm
  { _sku         :: Input SKU
  , _description :: Input (Maybe Description)
  , _serial      :: Input SerialNumber
  , _squadron    :: Dropdown 'One Squadron
  , _operable    :: Dropdown 'One Operable
  }


makeFieldsNoPrefix ''EditForm


emptyEditForm :: EditForm
emptyEditForm = EditForm
  { _sku         = Input Clean 0
  , _description = Input Clean Nothing
  , _serial      = Input Clean 0
  , _squadron    = fullOptions
  , _operable    = fullOptions
  }


data Frontend
  = MEcho (Maybe Text)
  | MList Roster
  | MDetail (Maybe SpaceCraftId) EditForm
  | M404


makeLenses ''Frontend


data Route
  = REcho (Maybe Text)
  | RList (Input Search)
  | RNew
  | RExisting SpaceCraftId
  deriving (Eq, Ord, Show, Generic)


makeLenses ''Route


type API = "api" :> "space-craft" :> Get '[JSON] (Set SpaceCraft)
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
    RList s     -> Redirect (Proxy @("app" :> QueryParam "search" Search :> Raw)) ($ Just (value s))


class CRUDSpaceCraft m where
  listSpaceCraft   :: m (Set SpaceCraft)
  getSpaceCraft    :: SpaceCraftId -> m (Maybe SpaceCraft)
  updateSpaceCraft :: SpaceCraftId -> SpaceCraftUpdate -> m ()
  createSpaceCraft :: SpaceCraftUpdate -> m SpaceCraftId
  deleteSpaceCraft :: SpaceCraftId -> m ()


instance Humanize (TableColumn (Set SpaceCraft)) where
  humanize = \case
    SKUT          -> "SKU"
    DescriptionT  -> "Desc"
    SerialNumberT -> "Serial #"
    SquadronT     -> "Squadron"
    OperableT     -> "Operable"


instance TableTerritory (Set SpaceCraft) where
  data TableColumn (Set SpaceCraft) =
    SKUT | DescriptionT | SerialNumberT | SquadronT | OperableT
    deriving (Eq, Ord, Show, Enum, Bounded)

  newtype TableRow (Set SpaceCraft) = Row { unRow :: SpaceCraft }
    deriving (Eq, Ord, Show)

  toRows = fmap Row . Set.toList
  toCell (Row SpaceCraft {..}) = \case
    SKUT          -> present _sku
    DescriptionT  -> maybe [text "N/A"] present _description
    SerialNumberT -> present _serial
    SquadronT     -> present _squadron
    OperableT     -> present _operable
  sortTable (SortCol c d) = f $ case c of
    SKUT          -> compare `on` Lens.view sku         . unRow
    DescriptionT  -> compare `on` Lens.view description . unRow
    SerialNumberT -> compare `on` Lens.view serial      . unRow
    SquadronT     -> compare `on` Lens.view squadron    . unRow
    OperableT     -> compare `on` Lens.view operable    . unRow
    where f = case d of ASC -> id; DESC -> flip
