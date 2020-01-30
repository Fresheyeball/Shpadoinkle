{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Types where


import           Control.Lens              as Lens
import           Control.Lens.TH           ()
import           Data.Aeson
import           Data.Function
import           Data.Maybe
import           Data.Proxy
import           Data.Set                  as Set
import           Data.String
import           Data.Text
import           GHC.Generics
import           Servant.API               hiding (Description)
import           Shpadoinkle
import           Shpadoinkle.Router
import           Shpadoinkle.Widgets.Table as Table
import           Shpadoinkle.Widgets.Types


newtype SKU          = SKU          { unSKU          :: Int  }
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON) deriving anyclass (Humanize, Present)


newtype Description  = Description  { unDescription  :: Text }
  deriving newtype (Eq, Ord, Show, IsString, ToJSON, FromJSON) deriving anyclass (Humanize, Present)


newtype SerialNumber = SerialNumber { unSerialNumber :: Int  }
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON) deriving anyclass (Humanize, Present)


newtype SpaceCraftId = SpaceCraftId { unSpaceCraftId :: Int }
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData) deriving anyclass (Humanize, Present)


data Operable = Inoperable | Operational       deriving (Eq, Ord, Show, Humanize, Present, Generic, ToJSON, FromJSON)
data Squadron = AwayTeam | StrikeForce | Scout deriving (Eq, Ord, Show, Present, Generic, ToJSON, FromJSON)
instance Humanize Squadron where
  humanize = \case
    AwayTeam    -> "Away Team"
    StrikeForce -> "Strike Force"
    Scout       -> "Scouting"


data SpaceCraft = SpaceCraft
  { _identity    :: SpaceCraftId
  , _sku         :: SKU
  , _description :: Maybe Description
  , _serial      :: SerialNumber
  , _squadron    :: Squadron
  , _operable    :: Operable
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


makeFieldsNoPrefix ''SpaceCraft


data SpaceCraftUpdate = SpaceCraftUpdate
  { _sku         :: Maybe SKU
  , _description :: Maybe Description
  , _serial      :: Maybe SerialNumber
  , _squadron    :: Maybe Squadron
  , _operable    :: Maybe Operable
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


makeFieldsNoPrefix ''SpaceCraftUpdate


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


data Frontend = Frontend
  { _table :: Set SpaceCraft
  , _sort  :: SortCol (Set SpaceCraft)
  , _route :: Route
  } deriving (Eq, Ord, Show)


data Route
  = Echo (Maybe Text)
  | RList (Input Search)
  | RNew
  | RExisting SpaceCraftId
  deriving (Eq, Ord, Show)


makeFieldsNoPrefix ''Frontend


type API = "api" :> "space-craft" :> Get '[JSON] [SpaceCraft]
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> Get '[JSON] SpaceCraft
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> ReqBody '[JSON] SpaceCraftUpdate :> Post '[JSON] ()
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] SpaceCraftUpdate :> Put '[JSON] SpaceCraftId
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] SpaceCraftId :> Delete '[JSON] ()


type SPA = "app" :> "echo" :> QueryParam "echo" Text :> Raw
      :<|> "app" :> "new"  :> Raw
      :<|> "app" :> "edit" :> Capture "id" SpaceCraftId :> Raw
      :<|> "app" :> QueryParam "search" Search :> Raw
      :<|> Raw


routes :: SPA :>> Route
routes = Echo
    :<|> RNew
    :<|> RExisting
    :<|> RList . Input Clean . fromMaybe ""
    :<|> RList (Input Clean "")


deriving newtype instance ToHttpApiData   Search
deriving newtype instance FromHttpApiData Search


instance Routed SPA Route where
  redirect = \case
    Echo t      -> Redirect (Proxy @("app" :> "echo" :> QueryParam "echo" Text :> Raw)) ($ t)
    RNew        -> Redirect (Proxy @("app" :> "new" :> Raw)) id
    RExisting i -> Redirect (Proxy @("app" :> "edit" :> Capture "id" SpaceCraftId :> Raw)) ($ i)
    RList s     -> Redirect (Proxy @("app" :> QueryParam "search" Search :> Raw)) ($ Just (value s))
