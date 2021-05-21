{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}


module Shpadoinkle.Website.Types.Hoogle where


import           Data.Aeson                              (FromJSON, ToJSON)
import           Data.Monoid.Generic                     (GenericMonoid (..),
                                                          GenericSemigroup (..))
import           GHC.Generics                            (Generic)
import           Shpadoinkle                             (NFData)
import           Shpadoinkle.Website.Types.Hoogle.Target (Target)
import           Shpadoinkle.Widgets.Form.Dropdown       (Dropdown)
import           Shpadoinkle.Widgets.Types               (Input, Pick (One),
                                                          Search)


data Hoogle = Hoogle
  { search  :: Input Search
  , targets :: Dropdown 'One Target
  }
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)
  deriving Semigroup via GenericSemigroup Hoogle
  deriving Monoid    via GenericMonoid Hoogle
