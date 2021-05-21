{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Shpadoinkle.Website.Types.Nav where


import           Shpadoinkle.Website.Types.Route                as R (Route (RGettingStarted, RHome, RTutorial))
import           Shpadoinkle.Website.Types.Route.GettingStarted (Route (RGSIndex))
import           Shpadoinkle.Website.Types.Route.Tutorial       (Route (RTIndex))
import           Shpadoinkle.Widgets.Types                      (Humanize (..))


data Nav
  = NHome
  | NGettingStarted
  | NTutorial
--  | NSandbox
  deriving (Eq, Ord, Enum, Bounded)


instance Humanize Nav where
  humanize = \case
    NHome           -> "Home"
    NGettingStarted -> "Get Started"
    NTutorial       -> "Tutorial"
--    NSandbox        -> "Sandbox"


toRoute :: Nav -> R.Route
toRoute = \case
  NHome           -> RHome
  NGettingStarted -> RGettingStarted RGSIndex
  NTutorial       -> RTutorial RTIndex
--  NSandbox        -> RSandbox


