{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Shpadoinkle.Website.Types.Nav where


import           Shpadoinkle.Website.Types.Route                as R (Route (RConcepts, RGettingStarted, RPackages, RTutorial))
import           Shpadoinkle.Website.Types.Route.GettingStarted (Route (RGSIndex))
import           Shpadoinkle.Website.Types.Route.Packages       (Route (RPIndex))
import           Shpadoinkle.Website.Types.Route.Tutorial       (Route (RTIndex))
import           Shpadoinkle.Widgets.Types                      (Humanize (..))
#ifdef TESTING
import           Test.QuickCheck                                (Arbitrary (..),
                                                                 arbitraryBoundedEnum)
#endif


data Nav
  = NConcept
  | NGettingStarted
  | NReference
  | NTutorial
  deriving (Show, Eq, Ord, Enum, Bounded)


instance Humanize Nav where
  humanize = \case
    NConcept        -> "Concept"
    NGettingStarted -> "Get Started"
    NReference      -> "Reference"
    NTutorial       -> "Tutorial"


toRoute :: Nav -> R.Route
toRoute = \case
  NConcept        -> RConcepts
  NGettingStarted -> RGettingStarted RGSIndex
  NReference      -> RPackages       RPIndex
  NTutorial       -> RTutorial       RTIndex


#ifdef TESTING
instance Arbitrary Nav where arbitrary = arbitraryBoundedEnum
#endif
