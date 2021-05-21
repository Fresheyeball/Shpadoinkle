{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}


module Shpadoinkle.Website.Types.Example where


import           Data.Aeson                             (FromJSON, ToJSON)
import           GHC.Generics                           (Generic)
import           Shpadoinkle                            (NFData)
import           Shpadoinkle.Isreal.Types               (Code, SnowNonce,
                                                         SnowToken)
import           Shpadoinkle.Website.Types.ExampleState (ExampleState)


data Example = Example
  { inputHaskell :: Code
  , snowToken    :: SnowToken
  , snowNonce    :: SnowNonce
  , state        :: ExampleState
  }
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)
