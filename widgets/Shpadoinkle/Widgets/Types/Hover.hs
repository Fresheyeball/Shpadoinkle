{-# LANGUAGE DeriveGeneric #-}


module Shpadoinkle.Widgets.Types.Hover ( HoverState (..), WithHoverState ) where


import Data.Aeson
import GHC.Generics


data HoverState = Hover | NotHover
  deriving (Eq, Show, Generic)

instance ToJSON HoverState
instance FromJSON HoverState


type WithHoverState a = (a, HoverState)
