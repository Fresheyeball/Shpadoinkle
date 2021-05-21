{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Shpadoinkle.Website.Types.Route.GettingStarted where


import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           Servant.API               (type (:<|>) ((:<|>)), type (:>))
import           Shpadoinkle               (NFData)
import           Shpadoinkle.Router        (View, type (:>>))
import           Shpadoinkle.Widgets.Types (Humanize (..))



data Route
  = RGSIndex
  | RGSAddingToYourProject
  | RGSExtendAnExample
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic, FromJSON, ToJSON, NFData)


type LegacySPA m a
  =    "index.html" :>                  View m a
  :<|> "extend-an-example.html" :>      View m a
  :<|> "adding-to-your-project.html" :> View m a


type SPA m a
  =                                View m a
  :<|> "adding-to-your-project" :> View m a
  :<|> "extend-an-example"      :> View m a


routes :: SPA m a :>> Route
routes
  =    RGSIndex
  :<|> RGSAddingToYourProject
  :<|> RGSExtendAnExample


instance Humanize Route where
  humanize = \case
    RGSIndex               -> "Getting Started"
    RGSAddingToYourProject -> "Adding to your project"
    RGSExtendAnExample     -> "Extend an example"
