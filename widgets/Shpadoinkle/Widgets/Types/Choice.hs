{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

#ifdef TESTING
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
#endif


module Shpadoinkle.Widgets.Types.Choice where


import           Control.Compactable              (Compactable (compact, filter, partition, separate))
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Foldable                    as F
import           Data.Functor.Classes             (Eq1 (..), Ord1 (..))
import           Data.Kind                        (Type)
import qualified Data.List.NonEmpty               as NE
import           Data.Proxy
import           Data.Set                         as Set
import           GHC.Generics                     (Generic)
#ifdef TESTING
import           Test.QuickCheck
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Classes.Hspec
import           Test.QuickCheck.Classes.Internal (eq1, func1, func2)
#endif


data Pick = One | AtleastOne | Many
  deriving (Eq, Ord, Show, Generic)


type family Selected (p :: Pick) (a :: Type) :: Type where
  Selected 'One        a = Maybe a
  Selected 'AtleastOne a = a
  Selected 'Many       a = Set a


data Choice (p :: Pick) a = Choice
  { _selected :: Selected p a
  , _options  :: Set a
  }


deriving instance (Show (Selected p a), Show a)        => Show (Choice p a)
deriving instance (Read (Selected p a), Read a, Ord a) => Read (Choice p a)
deriving instance (Eq   (Selected p a), Eq a)          => Eq   (Choice p a)
deriving instance (Ord  (Selected p a), Ord a)         => Ord  (Choice p a)
deriving instance Generic (Choice p a)
instance (ToJSON   (Selected p a), ToJSON   a)        => ToJSON   (Choice p a)
instance (FromJSON (Selected p a), FromJSON a, Ord a) => FromJSON (Choice p a)


class DemotePick (p :: Pick)    where demotePick :: Pick
instance DemotePick 'One        where demotePick = One
instance DemotePick 'AtleastOne where demotePick = AtleastOne
instance DemotePick 'Many       where demotePick = Many


instance Eq1 (Choice 'One) where
  liftEq f (Choice s o) (Choice s' o') = liftEq f s s' && liftEq f o o'
instance Eq1 (Choice 'AtleastOne) where
  liftEq f (Choice s o) (Choice s' o') = f s s' && liftEq f o o'
instance Eq1 (Choice 'Many) where
  liftEq f (Choice s o) (Choice s' o') = liftEq f s s' && liftEq f o o'


instance Ord1 (Choice 'One) where
  liftCompare f (Choice s o) (Choice s' o') = liftCompare f s s' `compare` liftCompare f o o'
instance Ord1 (Choice 'AtleastOne) where
  liftCompare f (Choice s o) (Choice s' o') = f s s' `compare` liftCompare f o o'
instance Ord1 (Choice 'Many) where
  liftCompare f (Choice s o) (Choice s' o') = liftCompare f s s' `compare` liftCompare f o o'


instance (Bounded a, Enum a) => Bounded (Choice 'AtleastOne a) where
  minBound = Choice minBound fullset
  maxBound = Choice maxBound fullset


instance (Bounded a, Enum a) => Enum (Choice 'AtleastOne a) where
  toEnum n = Choice (toEnum n) fullset
  fromEnum (Choice x _) = fromEnum x


instance Foldable (Choice 'One)         where foldr f x (Choice y xs) = Set.foldr f (Prelude.foldr f x y) xs
instance Foldable (Choice 'AtleastOne)  where foldr f x (Choice y xs) = Set.foldr f               (f y x) xs
instance Foldable (Choice 'Many)        where foldr f x (Choice y xs) = Set.foldr f     (Set.foldr f x y) xs


instance (Semigroup a, Ord a) => Semigroup (Choice 'One a)
  where Choice x xs <> Choice y ys = Choice (x <> y) (xs <> ys)

instance (Semigroup a, Ord a) => Semigroup (Choice 'AtleastOne a)
  where Choice x xs <> Choice y ys = Choice (x <> y) (xs <> ys)

instance (Semigroup a, Ord a) => Semigroup (Choice 'Many a)
  where Choice x xs <> Choice y ys = Choice (x <> y) (xs <> ys)
instance (Semigroup a, Ord a) => Monoid    (Choice 'One  a) where mempty = noselection (mempty :: Set a)
instance (Semigroup a, Ord a) => Monoid    (Choice 'Many a) where mempty = noselection (mempty :: Set a)


instance Compactable (Choice 'One) where
  compact (Choice x xs) = Choice (compact x) (compact xs)
  separate (Choice x xs) = let (l,r) = separate xs; (l',r') = separate x in (Choice l' l, Choice r' r)
  filter p (Choice x xs) = Choice (Control.Compactable.filter p x) $ Set.filter p xs
  partition p (Choice x xs) = let (l, r) = Set.partition p xs; (l',r') = Control.Compactable.partition p x in (Choice l' l, Choice r' r)


instance Compactable (Choice 'Many) where
  compact (Choice x xs) = Choice (compact x) (compact xs)
  separate (Choice x xs) = let (l,r) = separate xs; (l',r') = separate x in (Choice l' l, Choice r' r)
  filter p (Choice x xs) = Choice (Control.Compactable.filter p x) $ Set.filter p xs
  partition p (Choice x xs) = let (l, r) = Set.partition p xs; (l',r') = Control.Compactable.partition p x in (Choice l' l, Choice r' r)


-- | Laws:
-- @
-- a == b ==> toSet a == toSet b -- toSet is injective
-- toSet (smap f s) == fmap f (toSet s)
-- if valid s then Set.valid (toSet s)
-- @
class SetLike f where
  toSet :: Ord a => f a -> Set a
  smap  :: Ord b => (a -> b) -> f a -> f b
  valid :: Ord a => f a -> Bool

instance SetLike Set where
  toSet = id
  {-# INLINE toSet #-}
  smap  = Set.map
  {-# INLINE smap #-}
  valid = Set.valid

instance SetLike Maybe where
  toSet = maybe mempty Set.singleton
  {-# INLINE toSet #-}
  smap  = fmap
  {-# INLINE smap #-}
  valid = const True

instance SetLike (Choice 'One) where
  toSet (Choice x xs)  = toSet x <> xs
  smap f (Choice x xs) = Choice (f <$> x)     (Set.map f xs)
  valid (Choice _ xs)  = Set.valid xs

instance SetLike (Choice 'AtleastOne) where
  toSet (Choice x xs)  = Set.singleton x <> xs
  smap f (Choice x xs) = Choice (f x)         (Set.map f xs)
  valid (Choice _ xs)  = Set.valid xs

instance SetLike (Choice 'Many) where
  toSet (Choice x xs)  = toSet x <> xs
  smap f (Choice x xs) = Choice (Set.map f x) (Set.map f xs)
  valid (Choice x xs)  = Set.valid x && Set.valid xs


#ifdef TESTING
class
  ( forall v. Eq                v  => Eq        (f v)
  , forall w. Show              w  => Show      (f w)
  , forall x. Ord               x  => Ord       (f x)
  , forall y. (Ord y, Semigroup y) => Semigroup (f y)
  , forall z. (Ord z, Arbitrary z) => Arbitrary (f z)
  ) => Propable1Set f
instance
  ( forall v. Eq                v  => Eq        (f v)
  , forall w. Show              w  => Show      (f w)
  , forall x. Ord               x  => Ord       (f x)
  , forall y. (Ord y, Semigroup y) => Semigroup (f y)
  , forall z. (Ord z, Arbitrary z) => Arbitrary (f z)
  ) => Propable1Set f


type instance Justice SetLike f = Propable1Set f
instance Legal SetLike where legal' _ = setLikeLaws


newtype ApplyOrd f a = ApplyOrd { unApplyOrd :: f a }
deriving instance (forall x. Eq x   => Eq (f x),   Eq a)   => Eq   (ApplyOrd f a)
deriving instance (forall x. Show x => Show (f x), Show a) => Show (ApplyOrd f a)
deriving instance
  ( forall x. (Ord x, Arbitrary x) => Arbitrary (f x)
  , Ord a
  , Arbitrary a
  ) => Arbitrary (ApplyOrd f a)


setLikeLaws ::
  ( SetLike f
  , forall c. Eq                c  => Eq        (f c)
  , forall d. Show              d  => Show      (f d)
  , forall e. (Arbitrary e, Ord e) => Arbitrary (f e)
  ) => proxy f -> Laws
setLikeLaws p = Laws "SetLike"
  [ ("Composition", setFunctorComposition p)
  , ("Identity",    setFunctorIdentity    p)
  , ("Const",       setFunctorConst       p)
  ]


setFunctorComposition, setFunctorIdentity, setFunctorConst :: forall proxy f.
  ( SetLike f
  , forall a. Eq                a  => Eq        (f a)
  , forall a. Show              a  => Show      (f a)
  , forall a. (Arbitrary a, Ord a) => Arbitrary (f a)
  ) => proxy f -> Property

setFunctorComposition _ = property $ \(ApplyOrd (a :: f Integer)) ->
  eq1 (smap func2 (smap func1 a)) (smap (func2 . func1) a)

setFunctorIdentity _    = property $ \(ApplyOrd (a :: f Integer)) ->
  eq1 (smap id a) a

setFunctorConst _       = property $ \(ApplyOrd (a :: f Integer)) ->
  let (<$$) = smap . const in
  eq1 (smap (const 'X') a) ('X' <$$ a)


class
  ( Propable1Ord (f p)
  , Propable0 (Selected p Integer)
  , DemotePick p, PickToSet p
  , Selection f p
  ) => PropableChoice f p
instance
  ( Propable1Ord (f p)
  , Propable0 (Selected p Integer)
  , DemotePick p, PickToSet p
  , Selection f p
  ) => PropableChoice f p


type instance Justice (Selection f) p = PropableChoice f p
instance Legal (Selection f) where
  legal' (_ :: Proxy (Selection f)) (_ :: Proxy p) =
    let Laws _ laws = selectionLaws (Proxy @(f p))
    in   Laws ("Selection '" <> show (demotePick @p)) laws


selectionLaws :: forall proxy f (p :: Pick).
  ( Selection f p
  , PickToSet p
  , Propable1Ord (f p)
  , Propable0 (Selected p Integer)
  ) => proxy (f p) -> Laws
selectionLaws p = Laws "Selection"
  [ ("Selected is an option",                 selectionIsAnOption               p)
  , ("Selected and unselected are exclusive", selectedAndUnselectedAreExclusive p)
  , ("If we select something it's an option", selectIsAnOption                  p)
  , ("Idempotence select",                    selectIdempotence                 p)
  , ("Select selected identity",              selectSelectedIdentity            p)
  , ("Unselected withOptions identity",       unSelectedWithOptionsIdentity     p)
  , ("Selected is not unselected",            selectIsNotUnselected             p)
  , ("Retain preserves as much user selection as possible", retainPreserves     p)
  , ("Retain sets the values",                retainSets                        p)
  ]


selectionIsAnOption, selectedAndUnselectedAreExclusive
  :: forall proxy f (p :: Pick).
  ( Selection f p
  , PickToSet p
  , Propable1Ord (f p)
  ) => proxy (f p) -> Property
selectionIsAnOption _ = property $ \(c :: f p Integer) ->
  pickToSet @p (selected c) `isSubsetOf` toSet c

selectedAndUnselectedAreExclusive _ = property $ \(c :: f p Integer) ->
  pickToSet @p (selected c) `disjoint` unselected c


selectIsAnOption, selectIdempotence, selectIsNotUnselected
  :: forall proxy f (p :: Pick).
  ( Selection f p
  , Propable1Ord (f p)
  ) => proxy (f p) -> Property
selectIsAnOption _ = property $ \(c :: f p Integer) x ->
  x `member` toSet (select' c x)

selectIdempotence _ = property $ \(c :: f p Integer) x ->
  select' (select' c x) x == select' c x

selectIsNotUnselected _ = property $ \(c :: f p Integer) x ->
  not $ x `member` unselected (select' c x)


selectSelectedIdentity, unSelectedWithOptionsIdentity
  :: forall proxy f (p :: Pick).
  ( Selection f p
  , Propable0 (Selected p Integer)
  ) => proxy (f p) -> Property
selectSelectedIdentity _ = property $ \x ->
  selected (x `withOptions` Set.empty :: f p Integer) == x

unSelectedWithOptionsIdentity _ = property $ \x ->
  unselected (x `withOptions` Set.empty :: f p Integer ) == mempty


retainPreserves
  :: forall proxy f (p :: Pick).
  ( Selection f p
  , PickToSet p
  ) => proxy (f p) -> Property
retainPreserves _ = property $ \(xs' :: Set Integer) x' y' ->
  let x, y :: Selected p Integer
      x = pickToSelected @p x'
      y = pickToSelected @p y'

      xs :: Set Integer
      xs = Set.insert x' $ Set.insert y' xs'

      r :: f p Integer
      r = (x `withOptions` xs) `retain` (y `withOptions` xs)

   in pickToSet @p x `isSubsetOf` pickToSet @p @Integer (selected r)


retainSets
  :: forall proxy f (p :: Pick).
  ( Selection f p
  , Propable1Ord (f p)
  ) => proxy (f p) -> Property
retainSets _ = property $ \(c :: f p Integer) x ->
  toSet (retain c x) == toSet x

#endif


ftoSet :: (Ord a, Foldable g) => g a -> Set a
ftoSet = Set.fromList . F.toList


class    PickToSet (p :: Pick) where pickToSet :: Ord a => Selected p a -> Set a
instance PickToSet 'One        where pickToSet = toSet
instance PickToSet 'AtleastOne where pickToSet = Set.singleton
instance PickToSet 'Many       where pickToSet = toSet


class    PickToSelected (p :: Pick) where pickToSelected' :: Ord a => Proxy p -> a -> Selected p a
instance PickToSelected 'One        where pickToSelected' _ = Just
instance PickToSelected 'AtleastOne where pickToSelected' _ = id
instance PickToSelected 'Many       where pickToSelected' _ = Set.singleton


pickToSelected :: forall (p :: Pick) a. (Ord a, PickToSelected p) => a -> Selected p a
pickToSelected = pickToSelected' (Proxy @p)


class (SetLike (f p), PickToSelected p) => Selection f (p :: Pick) where
  select        :: Ord a => f p a -> Selected p a -> f p a
  unselected    :: Ord a => f p a -> Set a
  selected      :: Ord a => f p a -> Selected p a
  withOptions   :: (Foldable g, Ord a) => Selected p a -> g a -> f p a
  retain        :: Ord a => f p a -> f p a -> f p a


select' :: forall f (p :: Pick) a. (Selection f p, Ord a) => f p a -> a -> f p a
select' c = select c . pickToSelected @p


withOptions' :: forall f (p :: Pick) a g. (Selection f p, Ord a, Foldable g) => a -> g a -> f p a
withOptions' = withOptions . pickToSelected @p


retain' :: (Ord a, Deselection f p, Foldable g) => f p a -> g a -> f p a
retain' xs = retain xs . noselection


instance Selection Choice 'One where
  select (Choice w xs) y = Choice y (toSet w <> toSet y <> xs)
  unselected (Choice x xs) = maybe xs (`Set.delete` xs) x
  selected = _selected
  withOptions x (ftoSet -> xs) = Choice x $ maybe xs (`Set.insert` xs) x
  retain (Choice w _) xs = case w of
    Just x | Set.member x (toSet xs) -> w `withOptions` xs; _ -> xs


instance Selection Choice 'AtleastOne where
  select (Choice _ xs) y = Choice y (Set.insert y xs)
  unselected (Choice x xs) = Set.delete x xs
  selected = _selected
  withOptions x (ftoSet -> xs) = Choice x (Set.insert x xs)
  retain (Choice w _) xs = if Set.member w (toSet xs) then w `withOptions` xs else xs


instance Selection Choice 'Many where
  select (Choice x xs) y = Choice (y <> x) (y <> xs)
  unselected (Choice x xs) = Set.difference xs x
  selected = _selected
  withOptions x (ftoSet -> xs) = Choice x (x <> xs)
  retain (Choice x _) (Choice y ys) = Choice (Set.intersection x ys <> y) ys


class Selection f p => Deselection f (p :: Pick) where
  noselection :: (Foldable g, Ord a) => g a -> f p a
  deselect    :: Ord a => f p a -> f p a

instance Deselection Choice 'One where
  noselection = Choice Nothing . Set.fromList . F.toList
  deselect = flip select Nothing

instance Deselection Choice 'Many where
  noselection = Choice mempty . Set.fromList . F.toList
  deselect (Choice ys xs) = Choice mempty (ys <> xs)


next, nextLoop, prev, prevLoop :: (Selection f 'AtleastOne, Ord a) => f 'AtleastOne a -> f 'AtleastOne a
next     xs = maybe xs                     (select xs) . Set.lookupGT (selected xs) $ toSet xs
nextLoop xs = maybe (unsafeSelectFirst xs) (select xs) . Set.lookupGT (selected xs) $ toSet xs
prev     xs = maybe xs                     (select xs) . Set.lookupLT (selected xs) $ toSet xs
prevLoop xs = maybe (unsafeSelectLast xs)  (select xs) . Set.lookupLT (selected xs) $ toSet xs


selectAll :: Choice 'Many a -> Choice 'Many a
selectAll (Choice _ xs) = Choice xs xs


unsafeSelectFirst :: (Selection f p, Ord a) => f p a -> f p a
unsafeSelectFirst c = select' c . Set.findMin $ toSet c


unsafeSelectLast :: (Selection f p, Ord a) => f p a -> f p a
unsafeSelectLast c = select' c . Set.findMax $ toSet c


selectFirst :: (Selection f p, Ord a) => f p a -> Maybe (f p a)
selectFirst c = fmap (select' c) . Set.lookupMin $ toSet c


selectLast :: (Selection f p, Ord a) => f p a -> Maybe (f p a)
selectLast c = fmap (select' c) . Set.lookupMax $ toSet c


fullset :: (Bounded a, Enum a) => Set a
fullset = Set.fromDistinctAscList [minBound..maxBound]


fullOptions :: (Deselection f p, Bounded a, Enum a, Ord a) => f p a
fullOptions = noselection fullset


fullOptionsMin :: (Selection f p, Bounded a, Enum a, Ord a) => f p a
fullOptionsMin = fromNonEmpty $ minBound NE.:| [succ minBound..maxBound]


fullOptionsMax :: (Selection f p, Bounded a, Enum a, Ord a) => f p a
fullOptionsMax = fromNonEmpty $ maxBound NE.:| [minBound..pred maxBound]


fromNonEmpty :: (Selection f p, Ord a) => NE.NonEmpty a -> f p a
fromNonEmpty xs' = let (x NE.:| xs) = NE.sort xs' in x `withOptions'` Set.fromList xs


selectWhen :: (SetLike g, Selection f 'Many, Ord a) => (a -> Bool) -> g a -> Maybe (f 'Many a)
selectWhen p xs' = if sub == Set.empty then Nothing else Just (sub `withOptions` xs)
  where sub = Set.filter p xs
        xs = toSet xs'


selectFirstWhen :: (SetLike g, Deselection f p, Ord a) => (a -> Bool) -> g a -> Maybe (f p a)
selectFirstWhen p xs = if sub == Set.empty then Nothing else selectFirst $ noselection sub
  where sub = Set.filter p $ toSet xs


selectLastWhen :: (SetLike g, Deselection f p, Ord a) => (a -> Bool) -> g a -> Maybe (f p a)
selectLastWhen p xs = if sub == Set.empty then Nothing else selectLast $ noselection sub
  where sub = Set.filter p $ toSet xs


toList :: (SetLike f, Ord a) => f a -> [a]
toList = Set.toList . toSet


singleton :: (Selection f p, Ord a) => a -> f p a
singleton x = x `withOptions'` Set.singleton x


before :: (Selection f 'AtleastOne, Ord a) => f 'AtleastOne a -> Set a
before xs = Set.filter (< selected xs) $ toSet xs


unsafeSelectAt :: (SetLike g, Selection f 'AtleastOne, Ord a) => Int -> g a -> f 'AtleastOne a
unsafeSelectAt i xs' = let xs = toSet xs' in Set.elemAt i xs `withOptions'` xs


getIndex :: (Selection f 'AtleastOne, Ord a) => f 'AtleastOne a -> Int
getIndex xs = findIndex (selected xs) $ toSet xs


after :: (Selection f 'AtleastOne, Ord a) => f 'AtleastOne a -> Set a
after xs = Set.filter (> selected xs) $ toSet xs


size :: (SetLike g, Ord a) => g a -> Int
size = Set.size . toSet


insert :: (Selection f p, Ord a) => a -> f p a -> f p a
insert y xs = selected xs `withOptions` Set.insert y (toSet xs)


delete :: (Compactable (f p), Ord a) => a -> f p a -> f p a
delete y = Control.Compactable.filter (/= y)


addSelection :: (Selection f 'Many, Ord a) => a -> f 'Many a -> f 'Many a
addSelection y c = select c $ Set.singleton y


deselectMany :: (Compactable (f p), Ord a) => Set a -> f p a -> f p a
deselectMany y = Control.Compactable.filter (`Set.member` y)


#ifdef TESTING
instance (Ord a, Arbitrary a, Arbitrary (Selected p a)) => Arbitrary (Choice p a) where
  arbitrary = Choice <$> arbitrary <*> arbitrary
#endif
