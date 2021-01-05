{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-implicit-kind-vars #-}


module Test.QuickCheck.Classes.Hspec where


import           Control.Applicative     (Alternative)
import           Data.Kind
import           Data.Proxy

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Classes


toSpec :: Laws -> Spec
toSpec (Laws name ps) = describe name $
  () <$ traverse (\(name', p) -> it name' $ property p) ps


legal :: forall c a.
  ( Legal c, c a, Justice c a) => Spec
legal = toSpec $ legal' (Proxy @c) (Proxy @a)


class Legal (c :: k -> Constraint) where
  legal' :: forall (a :: k). (c a, Justice c a) => Proxy c -> Proxy a -> Laws


class
  ( forall a. Eq        a => Eq        (f a)
  , forall a. Show      a => Show      (f a)
  , forall a. Arbitrary a => Arbitrary (f a)
  ) => Propable1 f
instance
  ( forall a. Eq        a => Eq        (f a)
  , forall a. Show      a => Show      (f a)
  , forall b. Arbitrary b => Arbitrary (f b)
  ) => Propable1 f


class
  ( forall a. Eq        a => Eq        (f a)
  , forall a. Show      a => Show      (f a)
  , forall a. Ord       a => Ord       (f a)
  , forall g. (Ord g, Arbitrary g) => Arbitrary (f g)
  ) => Propable1Ord f
instance
  ( forall a. Eq        a => Eq        (f a)
  , forall a. Show      a => Show      (f a)
  , forall a. Ord       a => Ord       (f a)
  , forall c. (Ord c, Arbitrary c) => Arbitrary (f c)
  ) => Propable1Ord f


class    (Eq a, Show a, Arbitrary a) => Propable0 a
instance (Eq a, Show a, Arbitrary a) => Propable0 a
class    (Bounded a, Eq a, Show a, Arbitrary a) => Propable0Bounded a
instance (Bounded a, Eq a, Show a, Arbitrary a) => Propable0Bounded a


type family Justice
  (c :: k -> Constraint)
  (a :: k)
     :: Constraint


type instance Justice Monoid      a = Propable0 a
type instance Justice Semigroup   a = Propable0 a
type instance Justice Eq          a = Propable0 a
type instance Justice Enum        a = Propable0Bounded a
type instance Justice Ord         a = Propable0 a
type instance Justice Functor     f = Propable1 f
type instance Justice Applicative f = Propable1 f
type instance Justice Alternative f = Propable1 f
type instance Justice Monad       f = Propable1 f
type instance Justice Show        a = Propable0 a
-- type instance Justice Foldable    f = Propable1Ord f
type instance Justice Traversable f = Propable1 f

-- foldableLawsOrd :: forall proxy f.
--   ( Foldable f
--   , forall w. Eq        w => Eq   (f w)
--   , forall e. Show      e => Show (f e)
--   , forall s. Ord       s => Ord  (f s)
--   , forall q. (Arbitrary q, Ord q) => Arbitrary (f q))
--   => proxy f -> Laws
-- foldableLawsOrd = foldableLaws


instance Legal Monoid      where legal' _ = monoidLaws
instance Legal Semigroup   where legal' _ = semigroupLaws
instance Legal Eq          where legal' _ = eqLaws
instance Legal Enum        where legal' _ = boundedEnumLaws
instance Legal Ord         where legal' _ = ordLaws
instance Legal Functor     where legal' _ = functorLaws
instance Legal Applicative where legal' _ = applicativeLaws
instance Legal Alternative where legal' _ = alternativeLaws
instance Legal Monad       where legal' _ = monadLaws
instance Legal Show        where legal' _ = showLaws
-- instance Legal Foldable    where legal' _ = foldableLawsOrd
-- instance Legal Traversable where legal' _ = traversableLaws
