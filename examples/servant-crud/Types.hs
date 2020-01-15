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
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Types where


import           Control.Lens
import           Control.Lens.TH           ()
import           Data.Aeson
import           Data.Function
import           Data.Proxy
import           Data.Set                  as Set
import           Data.String
import           Data.Text
import           GHC.Generics
import           Servant.API               hiding (Description)
import           Shpadoinkle
import           Shpadoinkle.Router
import           Shpadoinkle.Widgets.Table
import           Shpadoinkle.Widgets.Types


newtype SKU          = SKU          { unSKU          :: Int  }
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON) deriving anyclass (Humanize, Present)
newtype Description  = Description  { unDescription  :: Text }
  deriving newtype (Eq, Ord, Show, IsString, ToJSON, FromJSON) deriving anyclass (Humanize, Present)
newtype SerialNumber = SerialNumber { unSerialNumber :: Int  }
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON) deriving anyclass (Humanize, Present)
newtype SpaceCraftId = SpaceCraftId { unSpaceCraftId :: Int }
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON, ToHttpApiData) deriving anyclass (Humanize, Present)


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
    SKUT          -> compare `on` view sku         . unRow
    DescriptionT  -> compare `on` view description . unRow
    SerialNumberT -> compare `on` view serial      . unRow
    SquadronT     -> compare `on` view squadron    . unRow
    OperableT     -> compare `on` view operable    . unRow
    where f = case d of ASC -> id; DESC -> flip


data Frontend = Frontend
  { _table :: Set SpaceCraft
  , _sort  :: SortCol (Set SpaceCraft)
  , _route :: Route
  } deriving (Eq, Ord, Show)


data Route
  = Root
  | Echo (Maybe Text)
  deriving (Eq, Ord, Show)


makeFieldsNoPrefix ''Frontend


type API = "api" :> "space-craft" :> Capture "id" SpaceCraftId :> Get '[JSON] SpaceCraft
      :<|> "api" :> "space-craft" :> Capture "id" SpaceCraftId :> ReqBody '[JSON] SpaceCraftUpdate :> Post '[JSON] ()
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] SpaceCraftUpdate :> Put '[JSON] SpaceCraftId
      :<|> "api" :> "space-craft" :> ReqBody '[JSON] SpaceCraftId :> Delete '[JSON] ()


type SPA = View
      :<|> "app" :> View
      :<|> "app" :> "echo" :> QueryParam "echo" Text :> View


routes :: SPA `RoutedAs` Route
routes = Root
    :<|> Root
    :<|> Echo


instance Routed SPA Route where
  redirect = \case
    Root   -> Redirect (Proxy @("app" :> View)) id
    Echo t -> Redirect (Proxy @("app" :> "echo" :> QueryParam "echo" Text :> View)) ($ t)
