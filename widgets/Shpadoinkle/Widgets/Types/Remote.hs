{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}


module Shpadoinkle.Widgets.Types.Remote where


import           GHC.Generics


data Remote e a
  = Success a
  | Failure e
  | Loading
  | NotAsked
  deriving (Eq, Ord, Generic, Functor)


instance Applicative (Remote e) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e
  Loading <*> _   = Loading
  _ <*> Loading   = Loading
  NotAsked <*> _  = NotAsked
  _ <*> NotAsked  = NotAsked


instance Monad (Remote e) where
  Success a >>= f = f a
  Failure e >>= _ = Failure e
  NotAsked  >>= _ = NotAsked
  Loading   >>= _ = Loading
