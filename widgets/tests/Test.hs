{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main where


import           Control.Applicative               (Alternative)
import           Data.Monoid                       (Sum)
import           Data.Set                          as Set

import           Test.Hspec
import           Test.QuickCheck.Classes.Hspec

import           Shpadoinkle.Widgets.Form.Dropdown
import           Shpadoinkle.Widgets.Types


instance Show (a -> b) where
  show _ = "(a -> b)"


main :: IO ()
main = hspec $ do

  describe "Set" $ legal @SetLike @Set


  describe "Toggle"  $ do
    legal @Eq        @Toggle
    legal @Ord       @Toggle
    legal @Show      @Toggle
    legal @Semigroup @Toggle
    legal @Monoid    @Toggle


  describe "Hover"   $ do
    legal @Eq        @Hover
    legal @Ord       @Hover
    legal @Show      @Hover
    legal @Semigroup @Hover
    legal @Monoid    @Hover


  describe "Hygiene" $ do
    legal @Eq        @Hygiene
    legal @Ord       @Hygiene
    legal @Show      @Hygiene
    legal @Semigroup @Hygiene
    legal @Monoid    @Hygiene


  describe "Remote"  $ do
    legal @Eq          @(Remote Int Int)
    legal @Ord         @(Remote Int Int)
    legal @Show        @(Remote Int Int)
    legal @Monoid      @(Remote Int (Sum Int))
    legal @Functor     @(Remote Int)
    legal @Applicative @(Remote Int)
    legal @Monad       @(Remote Int)
    legal @Alternative @(Remote Int)
    legal @Foldable    @(Remote Int)


  describe "Input" $ do
    legal @Eq          @(Input (Sum Int))
    legal @Ord         @(Input (Sum Int))
    legal @Show        @(Input (Sum Int))
    legal @Monoid      @(Input (Sum Int))
    legal @Functor     @Input
    legal @Applicative @Input
    legal @Monad       @Input
    legal @Foldable    @Input


  describe "Validated" $ do
    legal @Eq          @(Validated Int Int)
    legal @Ord         @(Validated Int Int)
    legal @Show        @(Validated Int Int)
    legal @Semigroup   @(Validated Int Int)
    legal @Functor     @(Validated Int)
    legal @Applicative @(Validated Int)
    legal @Monad       @(Validated Int)
    legal @Foldable    @(Validated Int)


  describe "Choice" $ do

    describe "'One" $ do
      legal @Eq         @(Choice   'One Int)
      legal @Ord        @(Choice   'One Int)
      legal @Show       @(Choice   'One Int)
      legal @Semigroup  @(Choice   'One (Sum Int))
      legal @Monoid     @(Choice   'One (Sum Int))
      legal @SetLike    @(Choice   'One)
      legal @Foldable   @(Choice   'One)
      legal @(Selection   Choice) @'One
      legal @(Deselection Choice) @'One

    describe "'AtleastOne" $ do
      legal @Eq         @(Choice   'AtleastOne Int)
      legal @Ord        @(Choice   'AtleastOne Int)
      legal @Show       @(Choice   'AtleastOne Int)
      legal @Semigroup  @(Choice   'AtleastOne (Sum Int))
      legal @SetLike    @(Choice   'AtleastOne)
      legal @Foldable   @(Choice   'AtleastOne)
      legal @(Selection   Choice) @'AtleastOne

    describe "'Many" $ do
      legal @Eq         @(Choice   'Many Int)
      legal @Ord        @(Choice   'Many Int)
      legal @Show       @(Choice   'Many Int)
      legal @Semigroup  @(Choice   'Many (Sum Int))
      legal @Monoid     @(Choice   'Many (Sum Int))
      legal @SetLike    @(Choice   'Many)
      legal @Foldable   @(Choice   'Many)
      legal @(Selection   Choice) @'Many
      legal @(Deselection Choice) @'Many


  describe "ConsideredChoice" $ do

    describe "'One" $ do
      legal @Eq         @(ConsideredChoice   'One Int)
      legal @Ord        @(ConsideredChoice   'One Int)
      legal @Show       @(ConsideredChoice   'One Int)
      legal @Semigroup  @(ConsideredChoice   'One (Sum Int))
      legal @Monoid     @(ConsideredChoice   'One (Sum Int))
      legal @SetLike    @(ConsideredChoice   'One)
      legal @Foldable   @(ConsideredChoice   'One)
      legal @(Selection   ConsideredChoice) @'One
      legal @(Deselection ConsideredChoice) @'One

    describe "'AtleastOne" $ do
      legal @Eq        @(ConsideredChoice   'AtleastOne Int)
      legal @Ord       @(ConsideredChoice   'AtleastOne Int)
      legal @Show      @(ConsideredChoice   'AtleastOne Int)
      legal @Semigroup @(ConsideredChoice   'AtleastOne (Sum Int))
      legal @SetLike   @(ConsideredChoice   'AtleastOne)
      legal @Foldable  @(ConsideredChoice   'AtleastOne)
      legal @(Selection  ConsideredChoice) @'AtleastOne

    describe "'Many" $ do
      legal @Eq         @(ConsideredChoice   'Many Int)
      legal @Ord        @(ConsideredChoice   'Many Int)
      legal @Show       @(ConsideredChoice   'Many Int)
      legal @Semigroup  @(ConsideredChoice   'Many (Sum Int))
      legal @Monoid     @(ConsideredChoice   'Many (Sum Int))
      legal @SetLike    @(ConsideredChoice   'Many)
      legal @Foldable   @(ConsideredChoice   'Many)
      legal @(Selection   ConsideredChoice) @'Many
      legal @(Deselection ConsideredChoice) @'Many


  describe "Dropdown" $ do

    describe "'One" $ do
      legal @Eq         @(Dropdown   'One Int)
      legal @Ord        @(Dropdown   'One Int)
      legal @Show       @(Dropdown   'One Int)
      legal @Semigroup  @(Dropdown   'One (Sum Int))
      legal @Monoid     @(Dropdown   'One (Sum Int))
      legal @SetLike    @(Dropdown   'One)
      legal @Foldable   @(Dropdown   'One)
      legal @(Selection   Dropdown) @'One
      legal @(Deselection Dropdown) @'One

    describe "'AtleastOne" $ do
      legal @Eq        @(Dropdown   'AtleastOne Int)
      legal @Ord       @(Dropdown   'AtleastOne Int)
      legal @Show      @(Dropdown   'AtleastOne Int)
      legal @Semigroup @(Dropdown   'AtleastOne (Sum Int))
      legal @SetLike   @(Dropdown   'AtleastOne)
      legal @Foldable  @(Dropdown   'AtleastOne)
      legal @(Selection  Dropdown) @'AtleastOne

    describe "'Many" $ do
      legal @Eq         @(Dropdown   'Many Int)
      legal @Ord        @(Dropdown   'Many Int)
      legal @Show       @(Dropdown   'Many Int)
      legal @Semigroup  @(Dropdown   'Many (Sum Int))
      legal @Monoid     @(Dropdown   'Many (Sum Int))
      legal @SetLike    @(Dropdown   'Many)
      legal @Foldable   @(Dropdown   'Many)
      legal @(Selection   Dropdown) @'Many
      legal @(Deselection Dropdown) @'Many
