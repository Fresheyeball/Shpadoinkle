{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}


module Shpadoinkle.Widgets.Types.Remote where


import           Control.Applicative
import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics


data Remote e a
  = Success a
  | Failure e
  | Loading
  | NotAsked
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable, ToJSON, FromJSON)


instance Applicative (Remote e) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure e <*> _         = Failure e
  _ <*> Failure e         = Failure e
  Loading <*> _           = Loading
  _ <*> Loading           = Loading
  NotAsked <*> _          = NotAsked
  _ <*> NotAsked          = NotAsked


instance Alternative (Remote e) where
   empty = NotAsked
   x@(Success _) <|> _ = x
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
