{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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


module Types (module Types, module Types.Prim) where


import           Control.Lens                      as Lens hiding (Context)
import           Control.Lens.TH                   ()
import           Control.Monad.Except
import           Data.Aeson
import           Data.Function
import           Data.Maybe
import           Data.Proxy
import           Data.Text
import           Database.Beam

import           Servant.API                       hiding (Description)
import           Shpadoinkle
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Router
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown
import           Shpadoinkle.Widgets.Table         as Table
import           Shpadoinkle.Widgets.Types
import           Shpadoinkle.Widgets.Validation

import           Types.Prim


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


makeLenses ''Frontend


-- TODO: can we get rid of this coproduct boilerplate? i.e. can we derive a coproduct representation?
type FrontendCoproduct = Either (Either (Either (Maybe Text) Roster)
                                 (Maybe SpaceCraftId, SpaceCraftUpdate 'Edit)) ()


frontendToCoproduct :: Frontend -> FrontendCoproduct
frontendToCoproduct = \case
  MEcho t -> Left (Left (Left t))
  MList r -> Left (Left (Right r))
  MDetail i e -> Left (Right (i, e))
  M404 -> Right ()


coproductToFrontend :: FrontendCoproduct -> Frontend
coproductToFrontend = \case
  Left (Left (Left t)) -> MEcho t
  Left (Left (Right r)) -> MList r
  Left (Right (i,e)) -> MDetail i e
  Right () -> M404


coproductIsoFrontend :: EndoIso FrontendCoproduct Frontend
coproductIsoFrontend = piiso coproductToFrontend frontendToCoproduct


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

  toCell _ (SpaceCraftRow SpaceCraft {..}) = \case
    SKUT          -> present _sku
    DescriptionT  -> present _description
    SerialNumberT -> present _serial
    SquadronT     -> present _squadron
    OperableT     -> present _operable
    ToolsT        ->
      [ H.div "btn-group"
        [ H.button [ H.class' "btn btn-sm btn-secondary",
                     H.onClick . causes $ navigate @ SPA (RExisting _identity) ] [ "Edit" ]
        , H.button [ H.class' "btn btn-sm btn-secondary",
                     H.onClick . impur $ do
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


