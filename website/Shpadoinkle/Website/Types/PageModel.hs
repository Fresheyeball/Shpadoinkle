{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Shpadoinkle.Website.Types.PageModel where


import           Data.Aeson                     (FromJSON, ToJSON)
import           GHC.Generics                   (Generic)
import           Shpadoinkle                    (NFData)
import           Shpadoinkle.Website.Types.Home (Home)


data PageModel
  = MHome Home
  | MStatic
  deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON, NFData)
