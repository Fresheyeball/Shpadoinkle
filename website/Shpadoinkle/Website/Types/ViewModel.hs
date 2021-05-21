{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Shpadoinkle.Website.Types.ViewModel where


import           Data.Aeson                          (FromJSON, ToJSON)
import           GHC.Generics                        (Generic)
import           Shpadoinkle                         (NFData)
import           Shpadoinkle.Website.Types.PageModel (PageModel)
import           Shpadoinkle.Website.Types.Route     (Route)
import           Shpadoinkle.Widgets.Types.Physical  (Toggle)


data ViewModel = ViewModel
  { mobileMenu   :: Toggle
  , currentRoute :: Route
  , pageModel    :: PageModel
  }
  deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON, NFData)
