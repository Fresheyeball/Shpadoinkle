{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}


module Shpadoinkle.Website.Types.ExampleState where


import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)
import           Shpadoinkle              (NFData)
import           Shpadoinkle.Isreal.Types (CompileError)


data ExampleState = EError CompileError | ELoading | EReady
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)
