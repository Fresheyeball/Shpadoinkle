{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}


module Shpadoinkle.Widgets.Types.Remote where


import           Control.Applicative
import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics
import           Shpadoinkle         (NFData)
#ifdef TESTING
import           Test.QuickCheck     (Arbitrary (..), elements)
#endif


data Remote e a
  = Success a
  | Failure e
  | Loading
  | NotAsked
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable, ToJSON, FromJSON, NFData)


instance Applicative (Remote e) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure e <*> _         = Failure e
  Loading <*> _           = Loading
  NotAsked <*> _          = NotAsked
  _ <*> Failure e         = Failure e
  _ <*> Loading           = Loading
  _ <*> NotAsked          = NotAsked


instance Alternative (Remote e) where
   empty = NotAsked
   x@(Success _) <|> _ = x
   NotAsked <|> x      = x
   x <|> NotAsked      = x
   _ <|> x             = x


instance Semigroup a => Semigroup (Remote e a) where
  Success x <> Success y = Success (x <> y)
  x <> y                 = x <|> y


instance Semigroup a => Monoid (Remote e a) where
  mempty = empty


instance Monad (Remote e) where
  Success a >>= f = f a
  Failure e >>= _ = Failure e
  NotAsked  >>= _ = NotAsked
  Loading   >>= _ = Loading


#ifdef TESTING
instance (Arbitrary e, Arbitrary a) => Arbitrary (Remote e a) where
  arbitrary = do
    (e, a) <- (,) <$> arbitrary <*> arbitrary
    elements [ Success a, Failure e, Loading, NotAsked ]
#endif
