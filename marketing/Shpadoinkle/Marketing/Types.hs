{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Shpadoinkle.Marketing.Types where


import           Control.Monad.Except               (MonadTrans (..))
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Monoid.Generic                (GenericMonoid (..),
                                                     GenericSemigroup (..))
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Servant.API                        (Capture,
                                                     FromHttpApiData (parseUrlPiece),
                                                     Get, JSON, QueryParam,
                                                     ToHttpApiData (toUrlPiece),
                                                     type (:<|>) (..),
                                                     type (:>))

import           Shpadoinkle.Isreal.Types           (Code, CompileError,
                                                     SnowToken)
import           Shpadoinkle.Router                 (HasRouter (..), View)
import           Shpadoinkle.Widgets.Types          (Input, Search (..))

import           Shpadoinkle.Marketing.Types.Hoogle (Target)


type Home = Hoogle


data Hoogle = Hoogle
  { search  :: Input Search
  , targets :: [Target]
  }
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving Semigroup via GenericSemigroup Home
  deriving Monoid    via GenericMonoid Home


data Comparison = Comparison
  { framework :: Framework
  , tokenMay  :: Maybe SnowToken
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data Route
  = HomeR
  | ComparisonR Framework
  | FourOhFourR


data Frontend
  = HomeM       Home
  | ComparisonM Comparison
  | FourOhFourM
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data Framework = React | Vue | Elm | Halogen | Reflex
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, FromJSON, ToJSON)


instance FromHttpApiData Framework where
  parseUrlPiece = \case
    "react"   -> pure React
    "vue"     -> pure Vue
    "elm"     -> pure Elm
    "halogen" -> pure Halogen
    "reflex"  -> pure Reflex
    x         -> Left $ x <> " is not a valid framework"


instance ToHttpApiData Framework where
  toUrlPiece = \case
    React   -> "react"
    Vue     -> "vue"
    Elm     -> "elm"
    Halogen -> "halogen"
    Reflex  -> "reflex"


type SPA m
     = "home" :> View m Frontend
  :<|> "comparisons" :> Capture "framework" Framework :> View m Frontend
  :<|> "404" :> View m Frontend
  :<|> View m Frontend


type HoogleAPI = QueryParam "mode"   Text
              :> QueryParam "hoogle" Text
              :> QueryParam "start"  Int
              :> QueryParam "count"  Int
              :> Get '[JSON] [Target]


class Swan m where
  token   :: m SnowToken
  compile :: SnowToken -> Code -> m (Either CompileError Text)
  clean   :: SnowToken -> m Text


instance (MonadTrans t, Monad m, Swan m) => Swan (t m) where
  token     = lift token
  compile x = lift . compile x
  clean     = lift . clean


class Hooglable m where
  findTargets :: Search -> m [Target]


instance (MonadTrans t, Monad m, Hooglable m) => Hooglable (t m) where
  findTargets = lift . findTargets


routes :: SPA m :>> Route
routes
     = HomeR
  :<|> ComparisonR
  :<|> FourOhFourR
  :<|> HomeR
