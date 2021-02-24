{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Shpadoinkle.Widgets.Types.Form
  ( module Shpadoinkle.Widgets.Types.Form
  ) where


import           Control.Applicative            (Alternative (empty),
                                                 Applicative (liftA2),
                                                 Const (Const, getConst))
import           Control.Monad.Except           (MonadError (..))
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Kind                      (Type)
import           Data.String                    (IsString)
import           Data.Text                      (Text)
import           GHC.Generics
#ifdef TESTING
import           Test.QuickCheck                (Arbitrary (..), elements)
#endif

import           Shpadoinkle                    (NFData)
import           Shpadoinkle.Widgets.Types.Core (Hygiene)


data Input a = Input
  { _hygiene :: Hygiene
  , _value   :: a
  } deriving (Eq, Ord, Show, Read, Functor, Traversable, Foldable, Generic, ToJSON, FromJSON, NFData)


#ifdef TESTING
instance Arbitrary a => Arbitrary (Input a) where arbitrary = Input <$> arbitrary <*> arbitrary
#endif


class Control g where
  type Val g a :: Type
  type Val g a = a
  hygiene ::  Applicative f => (Hygiene -> f Hygiene) -> g a -> f (g a)
  value   :: (Applicative f, Ord a) => (Val g a -> f (Val g a)) -> g a -> f (g a)


getValue :: (Ord a, Monoid (Val g a), Control g) => g a -> Val g a
getValue = getConst . value Const


getHygiene :: Control g => g a -> Hygiene
getHygiene = getConst . hygiene Const


instance Control Input where
  hygiene f i = (\h -> i { _hygiene = h }) <$> f (_hygiene i)
  value   f i = (\a -> i { _value   = a }) <$> f (_value i)


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
  deriving anyclass NFData


data Validated e a = Validated a | Invalid e [e]
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, NFData)


instance Semigroup (Validated e a) where
  Validated a <> Validated _   = Validated a
  Validated _ <> x             = x
  x <> Validated _             = x
  Invalid x xs <> Invalid y ys = Invalid x (xs <> (y:ys))


instance Applicative (Validated e) where
  Validated f <*> Validated a = Validated (f a)
  Invalid x xs <*> _          = Invalid x xs
  _ <*> Invalid x xs          = Invalid x xs
  pure = Validated


instance Monad (Validated e) where
  Validated a >>= f  = f a
  Invalid x xs >>= _ = Invalid x xs


instance MonadError e (Validated e) where
  throwError e = Invalid e []
  catchError (Invalid x _) f = f x
  catchError v _             = v


data Status = Edit | Rules | Valid | Errors


type family Field (s :: Status) (e :: Type) (f :: Type -> Type) (x :: Type) :: Type where
  Field 'Valid    _ _ a = a
  Field 'Errors   e _ a = Validated e a
  Field 'Edit     _ f a = f a
  Field 'Rules    e f a = Val f a -> Validated e a


class ValidateG rules edit errs where
  validateg :: rules a -> edit a -> errs a
instance ValidateG U1 U1 U1 where
  validateg _ _ = U1
instance (ValidateG a b c, ValidateG d e f)
  => ValidateG (a :*: d) (b :*: e) (c :*: f) where
  validateg (a :*: b) (c :*: d) = validateg a c :*: validateg b d
instance (ValidateG a b c, ValidateG d e f, Alternative (c :+: f))
  => ValidateG (a :+: d) (b :+: e) (c :+: f) where
  validateg (L1 a) (L1 b) = L1 $ validateg a b
  validateg (R1 a) (R1 b) = R1 $ validateg a b
  validateg _ _           = empty
instance ValidateG a b c
  => ValidateG (M1 i x a) (M1 i' x' b) (M1 i'' x'' c) where
  validateg (M1 a) (M1 b) = M1 $ validateg a b
instance (Control c, Monoid v, Val c a ~ v, Ord a)
  => ValidateG (K1 i (v -> b)) (K1 i' (c a)) (K1 i'' b) where
  validateg (K1 f) (K1 x) = K1 (f $ getValue x)


class ValidG err valid where
  validg :: err a -> Maybe (valid a)
instance ValidG U1 U1 where
  validg _ = Just U1
instance (ValidG a c, ValidG b d) => ValidG (a :*: b) (c :*: d) where
  validg (a :*: b) = liftA2 (:*:) (validg a) (validg b)
instance (ValidG a c, ValidG b d) => ValidG (a :+: b) (c :+: d) where
  validg (L1 a) = L1 <$> validg a
  validg (R1 a) = R1 <$> validg a
instance (ValidG a b) => ValidG (M1 i c a) (M1 i' c' b) where
  validg (M1 a) = M1 <$> validg a
instance ValidG (K1 i (Validated t a)) (K1 i' a) where
  validg (K1 (Validated a)) = Just $ K1 a
  validg _                  = Nothing


class Validate (f :: Status -> Type) where
  validate :: f 'Edit -> f 'Errors
  default validate
    :: Generic (f 'Edit)
    => Generic (f 'Rules)
    => Generic (f 'Errors)
    => ValidateG (Rep (f 'Rules)) (Rep (f 'Edit)) (Rep (f 'Errors))
    => f 'Edit -> f 'Errors
  validate edit = to $ validateg (from (rules @ f)) (from edit)

  getValid :: f 'Errors -> Maybe (f 'Valid)
  default getValid
    :: Generic (f 'Errors)
    => Generic (f 'Valid)
    => ValidG (Rep (f 'Errors)) (Rep (f 'Valid))
    => f 'Errors -> Maybe (f 'Valid)
  getValid = fmap to . validg . from

  rules :: f 'Rules


#ifdef TESTING
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validated a b) where
  arbitrary = do
    (e, es, a) <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    elements [ Validated  a, Invalid  e es ]
#endif
