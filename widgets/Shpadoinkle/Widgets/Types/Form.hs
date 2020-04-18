{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Shpadoinkle.Widgets.Types.Form
  (module Shpadoinkle.Widgets.Types.Form
  ) where


import           Control.Applicative
import           Data.Aeson
import           Data.Kind
import           Data.String
import           Data.Text                      hiding (empty)
import           GHC.Generics

import           Shpadoinkle.Widgets.Types.Core


data Input a = Input
  { _hygiene :: Hygiene
  , _value   :: a
  } deriving (Eq, Ord, Show, Read, Functor, Traversable, Foldable, Generic, ToJSON, FromJSON)


class Control g where
  hygiene :: Applicative f => (Hygiene -> f Hygiene) -> g a -> f (g a)
  value   :: Applicative f => (a -> f a) -> g a -> f (g a)


getValue :: (Monoid a, Control g) => g a -> a
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


data Status = Edit | Rules | Valid | Errors


type family Field (s :: Status) (f :: Type -> Type) (x :: Type) :: Type where
  Field 'Valid      _ a = a
  Field 'Errors     _ a = Either Text a
  Field 'Edit       f a = f a
  Field 'Rules      _ a = a -> Either Text a


nonMEmpty :: (Monoid a, Eq a) => a -> Either Text a
nonMEmpty x = if x == mempty then fail "Cannot be empty" else pure x


class ValidateG rules edit errs where
  validateg :: rules a -> edit a -> errs a
instance ValidateG U1 U1 U1 where
  validateg _ _ = U1
instance (ValidateG a b c, ValidateG d e f) => ValidateG (a :*: d) (b :*: e) (c :*: f) where
  validateg (a :*: b) (c :*: d) = validateg a c :*: validateg b d
instance (ValidateG a b c, ValidateG d e f, Alternative (c :+: f)) => ValidateG (a :+: d) (b :+: e) (c :+: f) where
  validateg (L1 a) (L1 b) = L1 $ validateg a b
  validateg (R1 a) (R1 b) = R1 $ validateg a b
  validateg _ _           = empty
instance ValidateG a b c => ValidateG (M1 i x a) (M1 i' x' b) (M1 i'' x'' c) where
  validateg (M1 a) (M1 b) = M1 $ validateg a b
instance (Control c, Monoid a) => ValidateG (K1 i (a -> b)) (K1 i' (c a)) (K1 i'' b) where
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
instance ValidG (K1 i (Either t a)) (K1 i' a) where
  validg (K1 (Right a)) = Just $ K1 a
  validg _              = Nothing


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
