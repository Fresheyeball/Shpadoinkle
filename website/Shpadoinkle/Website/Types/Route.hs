{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Shpadoinkle.Website.Types.Route where


import           Data.Aeson                                     (FromJSON,
                                                                 ToJSON)
import           GHC.Generics                                   (Generic)
import           Shpadoinkle                                    (NFData)
import qualified Shpadoinkle.Website.Types.Route.GettingStarted as GettingStarted
import qualified Shpadoinkle.Website.Types.Route.Packages       as Packages
import qualified Shpadoinkle.Website.Types.Route.Tutorial       as Tutorial


data Route
  = RHome
  | RConcepts
  | RGettingStarted GettingStarted.Route
  | RPackages       Packages.Route
  | RTutorial       Tutorial.Route
  | RSandbox
  | RFourOhFour
  deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON, NFData)
