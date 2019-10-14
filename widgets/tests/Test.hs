{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main where


import           Data.Kind
import           Data.Set                          as Set
import           GHC.Generics

import           Test.Hspec
import           Test.QuickCheck


import           Shpadoinkle.Widgets.Form.Dropdown
import           Shpadoinkle.Widgets.Types


data Foo = Bar | Baz | Qux | Nerp
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)


instance Semigroup Foo where (<>) = min
instance Monoid Foo where mempty = maxBound


instance Arbitrary Foo where
  arbitrary = arbitraryBoundedEnum

instance (Ord a, Arbitrary a, Arbitrary (Selected p a)) => Arbitrary (Choice p a) where
  arbitrary = Choice <$> arbitrary <*> arbitrary

instance (Ord a, Arbitrary a, Arbitrary (Selected p a), Arbitrary (Considered p a)) => Arbitrary (ConsideredChoice p a) where
  arbitrary = ConsideredChoice <$> arbitrary <*> arbitrary

instance (Ord a, Arbitrary a, Arbitrary (ConsideredChoice p a)) => Arbitrary (Dropdown p a) where
  arbitrary = Dropdown <$> arbitrary <*> arbitrary

instance Arbitrary Toggle where
  arbitrary = arbitraryBoundedEnum


type TestConstrant f p =
  ( Arbitrary (f p Foo)
  , Selection f p
  , Show (f p Foo)
  , Eq (f p Foo)
  )


type TestConstrants f =
  ( TestConstrant f 'One
  , TestConstrant f 'AtleastOne
  , TestConstrant f 'Many
  )


selectionProps :: forall (f :: Pick -> Type -> Type). TestConstrants f => Spec
selectionProps = do

  describe "selected is an option" $ do

    it "One"        . property $ \(c :: f 'One Foo) ->
      toSet (selected c) `isSubsetOf` toSet c
    it "AtleastOne" . property $ \(c :: f 'AtleastOne Foo) ->
      Set.singleton (selected c) `isSubsetOf` toSet c
    it "Many"       . property $ \(c :: f 'Many Foo) ->
      toSet (selected c) `isSubsetOf` toSet c


  describe "if we select something it's an option" $ do

    it "One'"        . property $ \(c :: f 'One Foo) x ->
      x `member` toSet (select' c x)
    it "AtleastOne'" . property $ \(c :: f 'AtleastOne Foo) x ->
      x `member` toSet (select' c x)
    it "Many'"       . property $ \(c :: f 'Many Foo) x ->
      x `member` toSet (select' c x)


  describe "selected and unselected are exclusive" $ do

    it "One"        . property $ \(c :: f 'One Foo) ->
      toSet (selected c) `disjoint` unselected c
    it "AtleastOne" . property $ \(c :: f 'AtleastOne Foo) ->
      Set.singleton (selected c) `disjoint` unselected c
    it "Many"       . property $ \(c :: f 'Many Foo) ->
      toSet (selected c) `disjoint` unselected c


  describe "idempotence select" $ do

    it "One"        . property $ \(c :: f 'One Foo) x ->
      select' (select' c x) x == select' c x
    it "AtleastOne" . property $ \(c :: f 'AtleastOne Foo) x ->
      select' (select' c x) x == select' c x
    it "Many"       . property $ \(c :: f 'Many Foo) x ->
      select' (select' c x) x == select' c x


  describe "select selected identity" $ do

    it "One"        . property $ \x ->
      selected (x `withOptions` Set.empty :: f 'One Foo) == x
    it "AtleastOne" . property $ \x ->
      selected (x `withOptions` Set.empty :: f 'AtleastOne Foo) == x
    it "Many"       . property $ \x ->
      selected (x `withOptions` Set.empty :: f 'Many Foo) == x


  describe "unselected withOptions identity" $ do

    it "One"        . property $ \x ->
      unselected (x `withOptions` Set.empty :: f 'One Foo) == mempty
    it "AtleastOne" . property $ \x ->
      unselected (x `withOptions` Set.empty :: f 'AtleastOne Foo) == mempty
    it "Many"       . property $ \x ->
      unselected (x `withOptions` Set.empty :: f 'Many Foo) == mempty


  describe "selected is not unselected" $ do

    it "One" . property $ \(c :: f 'One Foo) x ->
      not $ x `member` unselected (select' c x)
    it "AtleastOne" . property $ \(c :: f 'AtleastOne Foo) x ->
      not $ x `member` unselected (select' c x)
    it "Many" . property $ \(c :: f 'Many Foo) x ->
      not $ x `member` unselected (select' c x)


deselectionPropsOne :: forall (f :: Pick -> Type -> Type). TestConstrants f => Deselection f 'One => Spec
deselectionPropsOne = describe "One" $ do

    it "idempotence deselect" . property $ \(c :: f 'One Foo) ->
        deselect (deselect c) == deselect c

    it "deselect select selected identity" . property $ \(c :: f 'One Foo) x ->
        selected (select (deselect c) x) == x

    it "selected deselect annihliation" . property $ \(c :: f 'One Foo) ->
        selected (deselect c) == mempty

    it "deselect keeps" . property $ \(c :: f 'One Foo) x ->
        toSet x `isSubsetOf` toSet (deselect (select c x))

    it "unselected passes through deselect keeps" . property $ \(c :: f 'One Foo) x ->
        toSet x `isSubsetOf` unselected (deselect (select c x))

    it "deselect unselected is full set" . property $ \(c :: f 'One Foo) ->
        unselected (deselect c) == toSet c


deselectionPropsMany :: forall (f :: Pick -> Type -> Type). TestConstrants f => Deselection f 'Many => Spec
deselectionPropsMany = describe "Many" $ do

    it "idempotence deselect" . property $ \(c :: f 'Many Foo) ->
        deselect (deselect c) == deselect c

    it "deselect select selected identity" . property $ \(c :: f 'Many Foo) x ->
        selected (select (deselect c) x) == x

    it "selected deselect annihliation" . property $ \(c :: f 'Many Foo) ->
        selected (deselect c) == mempty

    it "deselect keeps" . property $ \(c :: f 'Many Foo) x ->
        toSet x `isSubsetOf` toSet (deselect (select c x))

    it "unselected passes through deselect keeps" . property $ \(c :: f 'Many Foo) x ->
        toSet x `isSubsetOf` unselected (deselect (select c x))

    it "deselect unselected is full set" . property $ \(c :: f 'Many Foo) ->
        unselected (deselect c) == toSet c


main :: IO ()
main = hspec $ do
  it "foooid" . property $ \(x :: Foo) y z ->
    x <> mempty == x && x == mempty <> x && (x <> y) <> z == x <> (y <> z)

  describe "Choice" $ do
    selectionProps       @Choice
    deselectionPropsOne  @Choice
    deselectionPropsMany @Choice


  describe "ConsideredChoice" $ do
    selectionProps       @ConsideredChoice
    deselectionPropsOne  @ConsideredChoice
    deselectionPropsMany @ConsideredChoice


  describe "Dropdown" $ do
    selectionProps      @Dropdown
    deselectionPropsOne @Dropdown
