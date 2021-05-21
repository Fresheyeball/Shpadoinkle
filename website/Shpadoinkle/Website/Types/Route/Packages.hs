{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Shpadoinkle.Website.Types.Route.Packages where


import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           Servant.API               (type (:<|>) ((:<|>)), type (:>))
import           Shpadoinkle               (NFData)
import           Shpadoinkle.Router        (View, type (:>>))
import           Shpadoinkle.Widgets.Types (Humanize (..))


data Route
  = RPIndex
  | RPCore
  | RPConsole
  | RPBackends
  | RPHtml
  | RPLens
  | RPRouter
  | RPWidgets
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic, FromJSON, ToJSON, NFData)


type LegacySPA m a
  =    "index.html"    :> View m a
  :<|> "core.html"     :> View m a
  :<|> "backends.html" :> View m a
  :<|> "console.html"  :> View m a
  :<|> "lens.html"     :> View m a
  :<|> "html.html"     :> View m a
  :<|> "router.html"   :> View m a
  :<|> "widgets.html"  :> View m a


type SPA m a
  =                  View m a
  :<|> "core"     :> View m a
  :<|> "console"  :> View m a
  :<|> "backends" :> View m a
  :<|> "html"     :> View m a
  :<|> "lens"     :> View m a
  :<|> "router"   :> View m a
  :<|> "widgets"  :> View m a


routes :: SPA m a :>> Route
routes
  =    RPIndex
  :<|> RPCore
  :<|> RPConsole
  :<|> RPBackends
  :<|> RPHtml
  :<|> RPLens
  :<|> RPRouter
  :<|> RPWidgets


instance Humanize Route where
  humanize = \case
    RPIndex    -> "Reference"
    RPCore     -> "Core"
    RPConsole  -> "Console"
    RPBackends -> "Backends"
    RPHtml     -> "Html"
    RPLens     -> "Lens"
    RPRouter   -> "Router"
    RPWidgets  -> "Widgets"
