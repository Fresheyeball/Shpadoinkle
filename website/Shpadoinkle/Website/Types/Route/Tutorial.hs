{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Shpadoinkle.Website.Types.Route.Tutorial where


import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           Servant.API               (type (:<|>) ((:<|>)), type (:>))
import           Shpadoinkle               (NFData)
import           Shpadoinkle.Router        (View, type (:>>))
import           Shpadoinkle.Widgets.Types (Humanize (..))


data Route
  = RTIndex
  | RTCalculator
  | RTImmediateExecution
  | RTComposing
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic, FromJSON, ToJSON, NFData)


type LegacySPA m a
  =    "index.html"               :> View m a
  :<|> "calculator.html"          :> View m a
  :<|> "immediate-execution.html" :> View m a
  :<|> "composing.html"           :> View m a


type SPA m a
  =                             View m a
  :<|> "calculator"          :> View m a
  :<|> "immediate-execution" :> View m a
  :<|> "composing"           :> View m a


routes :: SPA m a :>> Route
routes
  =    RTIndex
  :<|> RTCalculator
  :<|> RTImmediateExecution
  :<|> RTComposing


instance Humanize Route where
  humanize = \case
    RTIndex              -> "Tutorial"
    RTCalculator         -> "Calculator"
    RTImmediateExecution -> "Immediate Execution"
    RTComposing          -> "Composing"
