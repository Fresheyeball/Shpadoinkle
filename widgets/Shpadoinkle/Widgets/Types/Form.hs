{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Shpadoinkle.Widgets.Types.Form where


import           Data.Aeson
import           Data.String
import           Data.Text
import           GHC.Generics

import           Shpadoinkle.Widgets.Types.Core


data Input a = Input
  { _hygiene :: Hygiene
  , _value   :: a
  } deriving (Eq, Ord, Show, Read, Functor, Generic, ToJSON, FromJSON)


hygiene :: Applicative f => (Hygiene -> f Hygiene) -> Input a -> f (Input a)
hygiene f i = (\h -> i { _hygiene = h }) <$> f (_hygiene i)


value :: Applicative f => (a -> f a) -> Input a -> f (Input a)
value f i = (\a -> i { _value = a }) <$> f (_value i)


instance Applicative Input where
  Input h fa <*> Input h' a = Input (h <> h') (fa a)
  pure = Input mempty


instance Monad Input where
  Input h a >>= f = let Input h' a' = f a in Input (h <> h') a'


instance Semigroup a => Semigroup (Input a) where
  Input h a <> Input h' a' = Input (h <> h') (a <> a')


instance Monoid a => Monoid (Input a) where
  mempty = Input mempty mempty


newtype Placeholder = Placeholder { unPlaceholder :: Text }
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup, Monoid, ToJSON, FromJSON)
  deriving stock Generic


newtype Search = Search { unSearch :: Text }
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup, Monoid, ToJSON, FromJSON)
  deriving stock Generic

