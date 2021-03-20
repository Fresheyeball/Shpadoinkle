{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}


module Shpadoinkle.Marketing.Types where


import           Control.Monad.Except               (MonadTrans (..))
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.ByteString.Lazy               (fromStrict)
import           Data.FileEmbed
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

import           Shpadoinkle                        (NFData)
import           Shpadoinkle.Isreal.Types           (Code (..), CompileError,
                                                     SnowToken)
import           Shpadoinkle.Router                 (HasRouter (..), View)
import           Shpadoinkle.Widgets.Form.Dropdown
import           Shpadoinkle.Widgets.Types          (Input, Pick (One),
                                                     Search (..))

import           Shpadoinkle.Marketing.Types.Hoogle (Target)


newtype Examples = Examples
  { counter :: Example
  }
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)


data Example = Example
  { inputHaskell :: Code
  , snowToken    :: SnowToken
  }
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)


counterExample :: SnowToken -> Example
counterExample = Example
  (Code $ fromStrict $(embedFile "./counterExample.example"))


data Home = Home
  { hoogle   :: Hoogle
  , examples :: Examples
  }
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)


emptyHome :: SnowToken -> Home
emptyHome st = Home mempty (Examples $ counterExample st)


data Hoogle = Hoogle
  { search  :: Input Search
  , targets :: Dropdown 'One Target
  }
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)
  deriving Semigroup via GenericSemigroup Hoogle
  deriving Monoid via GenericMonoid Hoogle


data Comparison = Comparison
  { framework :: Framework
  , tokenMay  :: Maybe SnowToken
  }
  deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON, NFData)


data Route
  = HomeR
  | ComparisonR Framework
  | FourOhFourR


data Frontend
  = HomeM       Home
  | ComparisonM Comparison
  | FourOhFourM
  deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON, NFData)


data Framework = React | Vue | Elm | Halogen | Reflex
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Generic, FromJSON, ToJSON, NFData)


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
  compile :: SnowToken -> Code -> m (Either CompileError Text)
  clean   :: SnowToken -> m Text


instance (MonadTrans t, Monad m, Swan m) => Swan (t m) where
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
