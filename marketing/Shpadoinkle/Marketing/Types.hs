{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Shpadoinkle.Marketing.Types where


import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           Servant.API              (Capture,
                                           FromHttpApiData (parseUrlPiece),
                                           ToHttpApiData (toUrlPiece),
                                           type (:<|>) (..), type (:>))

import           Shpadoinkle.Isreal.Types (Code, CompileError, SnowToken)
import           Shpadoinkle.Router       (HasRouter (..), View)


type Home       = ()
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


class Swan m where
  token   :: m SnowToken
  compile :: SnowToken -> Code -> m (Either CompileError Text)
  clean   :: SnowToken -> m Text


routes :: SPA m :>> Route
routes
     = HomeR
  :<|> ComparisonR
  :<|> FourOhFourR
  :<|> HomeR
