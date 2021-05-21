{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}


module Shpadoinkle.Website.Types.Home where


import           Control.Lens                           (Lens', (^.))
import           Data.Aeson                             (FromJSON, ToJSON)
import           Data.ByteString.Lazy                   (fromStrict)
import           Data.FileEmbed                         (embedFile)
import           Data.Generics.Labels                   ()
import           GHC.Generics                           (Generic)
import           Shpadoinkle                            (NFData)
import           Shpadoinkle.Isreal.Types               (Code (..), SnowToken)
import           Shpadoinkle.Website.Types.Example      (Example (..))
import           Shpadoinkle.Website.Types.ExampleState (ExampleState (ELoading))
import           Shpadoinkle.Website.Types.Hoogle       (Hoogle)


data Home = Home
  { hoogle   :: Hoogle
  , examples :: Examples Example
  }
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)


data Examples a = Examples
  { helloWorld :: a
  , counter    :: a
  , todo       :: a
  }
  deriving stock    (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)


type ExampleLens = forall a. Lens' (Examples a) a


emptyExample :: Code -> SnowToken -> Example
emptyExample cc st = Example cc st 0 ELoading


emptyHome :: Examples SnowToken -> Home
emptyHome st = Home mempty $ Examples
  { helloWorld = emptyExample helloWorldExample $ st ^. #helloWorld
  , counter    = emptyExample counterExample    $ st ^. #counter
  , todo       = emptyExample todoExample       $ st ^. #todo
  }


helloWorldExample, counterExample, todoExample :: Code
helloWorldExample = Code $ fromStrict $(embedFile "./hello-world.example")
counterExample    = Code $ fromStrict $(embedFile "./counter.example")
todoExample       = Code $ fromStrict $(embedFile "./todo.example")
