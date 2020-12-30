{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
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
import           Shpadoinkle.Widgets.Types.Remote  as Remote


data Foo = Bar | Baz | Qux | Nerp | Floom | Donk
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)


instance Semigroup Foo where (<>) = min
instance Monoid    Foo where mempty = maxBound


instance Arbitrary   Foo where arbitrary = arbitraryBoundedEnum
instance CoArbitrary Foo


instance Show (a -> b) where
  show _ = "(a -> b)"

instance (Ord a, Arbitrary a, Arbitrary (Selected p a)) => Arbitrary (Choice p a) where
  arbitrary = Choice <$> arbitrary <*> arbitrary

instance (Ord a, Arbitrary a, Arbitrary (Selected p a), Arbitrary (Considered p a)) => Arbitrary (ConsideredChoice p a) where
  arbitrary = ConsideredChoice <$> arbitrary <*> arbitrary

instance (Ord a, Arbitrary a, Arbitrary (ConsideredChoice p a)) => Arbitrary (Dropdown p a) where
  arbitrary = Dropdown <$> arbitrary <*> arbitrary


instance Arbitrary Toggle  where arbitrary = arbitraryBoundedEnum
instance Arbitrary Hover   where arbitrary = arbitraryBoundedEnum
instance Arbitrary Hygiene where arbitrary = arbitraryBoundedEnum
instance Arbitrary a => Arbitrary (Input a) where arbitrary = Input <$> arbitrary <*> arbitrary
instance (Arbitrary e, Arbitrary a) => Arbitrary (Remote e a) where
  arbitrary = do
    (e, a) <- (,) <$> arbitrary <*> arbitrary
    elements [ Remote.Success a, Remote.Failure e, Loading, NotAsked ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validated a b) where
  arbitrary = do
    (e, es, a) <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    elements [ Validated  a, Invalid  e es ]


type TestConstraints a =
  ( Arbitrary a
  , Show      a
  , Eq        a
  )


type ChoiceTest f =
  ( ChoiceWith Arbitrary f
  , ChoiceWith Show      f
  , ChoiceWith Eq        f
  )


type ChoiceClass
  (c :: (Pick -> Type -> Type) -> Pick -> Constraint)
  (f :: Pick -> Type -> Type) =
  ( c f 'One
  , c f 'AtleastOne
  , c f 'Many
  )


type ChoiceWith (c :: Type -> Constraint) f =
  ( c (f 'One Foo)
  , c (f 'AtleastOne Foo)
  , c (f 'Many Foo)
  )


selectionProps :: forall (f :: Pick -> Type -> Type). (ChoiceTest f, ChoiceClass Selection f) => Spec
selectionProps = do
  selectionProps' @f @'One
  selectionProps' @f @'AtleastOne
  selectionProps' @f @'Many


selectionProps'
  :: forall (f :: Pick -> Type -> Type) (p :: Pick)
   . ( Selection f p
     , TestConstraints (f p Foo)
     , TestConstraints (Selected p Foo)
     , PickToSet p
     ) => Spec
selectionProps' = do

  it "selected is an option" . property $ \(c :: f p Foo) ->
      pickToSet @p (selected c) `isSubsetOf` toSet c

  it "selected and unselected are exclusive" . property $ \(c :: f p Foo) ->
      pickToSet @p (selected c) `disjoint` unselected c

  it "if we select something it's an option" . property $ \(c :: f p Foo) x ->
    x `member` toSet (select' c x)

  it "idempotence select" . property $ \(c :: f p Foo) x ->
    select' (select' c x) x == select' c x

  it "select selected identity" . property $ \x ->
    selected (x `withOptions` Set.empty :: f p Foo) == x

  it "unselected withOptions identity" . property $ \x ->
    unselected (x `withOptions` Set.empty :: f p Foo) == mempty

  it "selected is not unselected" . property $ \(c :: f p Foo) x ->
    not $ x `member` unselected (select' c x)

  it "retain preserves as much user selection as possible" . property $ \(xs' :: Set Foo) x' y' ->
    let x, y :: Selected p Foo
        x = toSelected @f @p x'
        y = toSelected @f @p y'

        xs :: Set Foo
        xs = Set.insert x' $ Set.insert y' xs'

        r :: f p Foo
        r = (x `withOptions` xs) `retain` (y `withOptions` xs)

     in pickToSet @p x `isSubsetOf` pickToSet @p @Foo (selected r)

  it "retain sets the values" . property $ \(c :: f p Foo) x ->
    toSet (retain c x) == toSet x


deselectionProps
  :: forall (f :: Pick -> Type -> Type)
   . ( TestConstraints (f 'One Foo)
     , TestConstraints (f 'Many Foo)
     , Deselection f 'One
     , Deselection f 'Many
     ) => Spec
deselectionProps = do
  deselectionProps' @f @'One
  deselectionProps' @f @'Many


deselectionProps'
  :: forall (f :: Pick -> Type -> Type) (p :: Pick) (s :: Type -> Type)
   . ( TestConstraints (f p Foo)
     , TestConstraints (s Foo)
     , Deselection f p
     , Selected p Foo ~ s Foo, SetLike s, Monoid (s Foo)
     ) => Spec
deselectionProps' = describe "Deselection" $ do

    it "idempotence deselect" . property $ \(c :: f p Foo) ->
      deselect (deselect c) == deselect c

    it "deselect select selected identity" . property $ \(c :: f p Foo) x ->
      selected (select (deselect c) x) == x

    it "selected deselect annihliation" . property $ \(c :: f p Foo) ->
      selected (deselect c) == mempty

    it "deselect keeps" . property $ \(c :: f p Foo) x ->
      toSet x `isSubsetOf` toSet (deselect (select c x))

    it "unselected passes through deselect keeps" . property $ \(c :: f p Foo) (x :: Selected p Foo) ->
      toSet x `isSubsetOf` unselected (deselect (select c x))

    it "deselect unselected is full set" . property $ \(c :: f p Foo) ->
      unselected (deselect c) == toSet c


setLike :: forall f. (TestConstraints (f Foo), SetLike f) => Spec
setLike = describe "SetLike" $ do
  it "functor composition" . property $ \(f :: Foo -> Foo) (g :: Foo -> Foo) (x :: f Foo) ->
    smap (f . g) x == smap f (smap g x)

  it "functor id" . property $ \(x :: f Foo) -> smap id x == x

  it "validity"   . property $ \(x :: f Foo) ->
    Shpadoinkle.Widgets.Types.valid x == Set.valid (toSet x)



horse :: forall f. (TestConstraints (f Foo), Semigroup (f Foo)) => Spec
horse =
  it "horse" . property $ \(x :: f Foo) -> (x <> x) == x


functor
  :: forall f
   . ( Functor f
     , TestConstraints (f Foo)
     , TestConstraints (f Char)
     ) => Spec
functor = describe "Functor" $ do
  it "composition" . property $ \(f :: Int -> Char) (g :: Foo -> Int) (x :: f Foo) ->
    fmap (f . g) x == fmap f (fmap g x)

  it "id" . property $ \(x :: f Foo) -> fmap id x == x


applicative
  :: forall f
   . ( Applicative f
     , Arbitrary (f (Foo -> Char))
     , Arbitrary (f (Int -> Char))
     , Arbitrary (f (Foo -> Int))
     , Show (f (Foo -> Char))
     , Show (f (Int -> Char))
     , Show (f (Foo -> Int))
     , TestConstraints (f Foo)
     , TestConstraints (f Char)
     ) => Spec
applicative = describe "Applicative" $ do
  functor @f

  it "identity"     . property $ \(x :: f Foo) ->
    (pure id <*> x) == x

  it "homomorphism" . property $ \(f :: Foo -> Char) (x :: Foo) ->
    (pure f <*> pure x) == (pure (f x) :: f Char)

  it "interchange" . property $ \(y :: Foo) (u :: f (Foo -> Char)) ->
    (u <*> pure y) == (pure ($ y) <*> u)

  it "composition" . property $ \u (v :: f (Foo -> Int)) (w :: f Foo) ->
    (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w) :: f Char)


monad
  :: forall m
   . ( Monad m
     , Arbitrary (m (Foo -> Char))
     , Arbitrary (m (Int -> Char))
     , Arbitrary (m (Foo -> Int))
     , Arbitrary (m Int)
     , Show (m (Foo -> Char))
     , Show (m (Int -> Char))
     , Show (m (Foo -> Int))
     , TestConstraints (m Foo)
     , TestConstraints (m Char))
      => Spec
monad = describe "Monad" $ do
  applicative @m

  it "left identity" . property $ \f (a :: Foo) ->
    (return a >>= f) == (f a :: m Char)

  it "right identity" . property $ \(m :: m Foo) ->
    (m >>= return) == m

  it "associativity" . property  $ \(m :: m Foo) (f :: Foo -> m Int) (g :: Int -> m Char) ->
    ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))


semigroup :: forall x. (TestConstraints x, Semigroup x) => Spec
semigroup = describe "Semigroup" $
  it "associativity" . property $ \(x :: x) y z ->
    (x <> y) <> z == x <> (y <> z)


monoid :: forall x. (Arbitrary x, Eq x, Show x, Monoid x) => Spec
monoid = describe "Monoid" $ do
  semigroup @x
  it "identity" . property $ \(x :: x) ->
    x <> mempty == x && x == mempty <> x


choiceMonoid
  :: forall (f :: Pick -> Type -> Type)
   . ( ChoiceTest f
     , Monoid (f 'One Foo)
     , Monoid (f 'Many Foo)
     , Semigroup (f 'AtleastOne Foo)
     ) => Spec
choiceMonoid = do
  monoid    @(f 'One  Foo)
  semigroup @(f 'AtleastOne Foo)
  monoid    @(f 'Many Foo)


choiceSetLike
  :: forall (f :: Pick -> Type -> Type)
   . ( ChoiceTest f
     , SetLike (f 'One)
     , SetLike (f 'AtleastOne)
     , SetLike (f 'Many)
     , Semigroup (f 'One Foo)
     , Semigroup (f 'AtleastOne Foo)
     , Semigroup (f 'Many Foo)
     ) => Spec
choiceSetLike = do

  setLike @(f 'One)
  setLike @(f 'AtleastOne)
  setLike @(f 'Many)

  horse @(f 'One)
  horse @(f 'AtleastOne)
  horse @(f 'Many)


main :: IO ()
main = hspec $ do


  describe "Foo"     $ monoid @Foo
  describe "Toggle"  $ monoid @Toggle
  describe "Hover"   $ monoid @Hover
  describe "Hygiene" $ monoid @Hygiene


  describe "Remote"  $ do
    monoid @(Remote Foo Foo)
    monad  @(Remote Foo)


  describe "Input" $ do
    monoid @(Input Foo)
    monad  @Input


  describe "Validated" $ do
    semigroup @(Validated Foo Foo)
    monad     @(Validated Foo)


  describe "Choice" $ do
    selectionProps   @Choice
    deselectionProps @Choice
    choiceSetLike    @Choice
    choiceMonoid     @Choice


  describe "ConsideredChoice" $ do
    selectionProps   @ConsideredChoice
    deselectionProps @ConsideredChoice
    choiceSetLike    @ConsideredChoice
    choiceMonoid     @ConsideredChoice


  describe "Dropdown" $ do
    selectionProps   @Dropdown
    deselectionProps @Dropdown
    choiceSetLike    @Dropdown
    choiceMonoid     @Dropdown


{-# ANN module ("HLint: ignore" :: String) #-}
