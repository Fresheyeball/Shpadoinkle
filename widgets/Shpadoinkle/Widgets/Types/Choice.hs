{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


module Shpadoinkle.Widgets.Types.Choice where


import           Control.Compactable
import           Data.Kind
import qualified Data.List.NonEmpty  as NE
import           Data.Set            as Set
import           GHC.Generics


data Pick   = One | AtleastOne | Many


type family Selected (p :: Pick) (a :: Type) :: Type where
  Selected 'One        a = Maybe a
  Selected 'AtleastOne a = a
  Selected 'Many       a = Set a


data Choice (p :: Pick) a = Choice
  { selected :: Selected p a
  , options  :: Set a
  }


deriving instance (Show (Selected p a), Show a)        => Show (Choice p a)
deriving instance (Read (Selected p a), Read a, Ord a) => Read (Choice p a)
deriving instance (Eq   (Selected p a), Eq a)          => Eq   (Choice p a)
deriving instance (Ord  (Selected p a), Ord a)         => Ord  (Choice p a)
deriving instance Generic (Choice p a)


instance (Bounded a, Enum a) => Bounded (Choice 'AtleastOne a) where
  minBound = Choice minBound fullset
  maxBound = Choice maxBound fullset


instance (Bounded a, Enum a) => Enum (Choice 'AtleastOne a) where
  toEnum n = Choice (toEnum n) fullset
  fromEnum (Choice x _) = fromEnum x


instance Foldable (Choice 'One)         where foldr f x (Choice y xs) = Set.foldr f (Prelude.foldr f x y) xs
instance Foldable (Choice 'AtleastOne)  where foldr f x (Choice y xs)  = Set.foldr f (f y x) xs
instance Foldable (Choice 'Many)        where foldr f x (Choice y xs)  = Set.foldr f (Set.foldr f x y) xs


instance Ord a => Semigroup (Choice 'One a)  where
  Choice (Just x) _ <> Choice _ ys | Set.member x ys = Choice (Just x) ys
  _                 <> y           = y
instance Ord a => Semigroup (Choice 'AtleastOne a)  where Choice x _ <> Choice y ys = if Set.member x ys then Choice x ys else Choice y ys
instance Ord a => Semigroup (Choice 'Many a) where Choice x _ <> Choice y ys = Choice (Set.intersection x ys <> y) ys
instance Ord a => Monoid    (Choice 'One a)  where mempty = emptyFromSet mempty


instance Compactable (Choice 'One) where
  compact (Choice x xs) = Choice (compact x) (compact xs)
  separate (Choice x xs) = let (l,r) = separate xs; (l',r') = separate x in (Choice l' l, Choice r' r)
  filter p (Choice x xs) = Choice (Control.Compactable.filter p x) $ Set.filter p xs
  partition p (Choice x xs) = let (l, r) = Set.partition p xs; (l',r') = Control.Compactable.partition p x in (Choice l' l, Choice r' r)


class SetMap (p :: Pick)    where smap :: Ord b => (a -> b) -> Choice p a -> Choice p b
instance SetMap 'One        where smap f (Choice x xs) = Choice (f <$> x)     (Set.map f xs)
instance SetMap 'AtleastOne where smap f (Choice x xs) = Choice (f x)         (Set.map f xs)
instance SetMap 'Many       where smap f (Choice x xs) = Choice (Set.map f x) (Set.map f xs)


class Selection (p :: Pick) where
  select :: Ord a => Choice p a -> Selected p a -> Choice p a
  select1 :: Ord a => Choice p a -> a -> Choice p a

instance Selection 'One where
  select (Choice _ xs) y = Choice y (maybe xs (`Set.insert` xs) y)
  select1 c = select c . Just

instance Selection 'AtleastOne where
  select (Choice _ xs) y = Choice y (Set.insert y xs)
  select1 = select

instance Selection 'Many where
  select (Choice x xs) y = Choice (y <> x) (y <> xs)
  select1 c = select c . Set.singleton


class WithOptions  (p :: Pick)   where withOptions :: Ord a => Selected p a -> Set a -> Choice p a
instance WithOptions 'AtleastOne where withOptions x xs = Choice x (Set.insert x xs)
instance WithOptions 'Many       where withOptions x xs = Choice x (x <> xs)


class Valid (p :: Pick)    where valid :: Ord a => Choice p a -> Bool
instance Valid 'One        where valid (Choice x xs) = Set.valid xs && maybe True (`Set.member` xs) x
instance Valid 'AtleastOne where valid (Choice x xs)  = Set.member x xs && Set.valid xs
instance Valid 'Many       where valid (Choice x xs)  = x `Set.isSubsetOf` xs && Set.valid x && Set.valid xs


class Deselection (p :: Pick) where deselect :: Ord a => Choice p a -> Choice p a
instance Deselection 'One where deselect (Choice _ xs) = Choice Nothing xs
instance Deselection 'Many where deselect (Choice _ xs) = Choice mempty xs


next, nextLoop, prev, prevLoop :: Ord a => Choice 'AtleastOne a -> Choice 'AtleastOne a
next     c@(Choice x xs) = maybe c                                (select c) $ Set.lookupGT x xs
nextLoop c@(Choice x xs) = maybe (unsafeSelectFirst c) (select c) $ Set.lookupGT x xs
prev     c@(Choice x xs) = maybe c                                (select c) $ Set.lookupLT x xs
prevLoop c@(Choice x xs) = maybe (unsafeSelectLast c)  (select c) $ Set.lookupLT x xs


selectAll :: Choice 'Many a -> Choice 'Many a
selectAll (Choice _ xs) = Choice xs xs


unsafeSelectFirst :: (Selection p, Ord a) => Choice p a -> Choice p a
unsafeSelectFirst c = select1 c . Set.findMin $ options c


unsafeSelectLast :: (Selection p, Ord a) => Choice p a -> Choice p a
unsafeSelectLast c = select1 c . Set.findMax $ options c


selectFirst :: (Selection p, Ord a) => Choice p a -> Maybe (Choice p a)
selectFirst c = fmap (select1 c) . Set.lookupMin $ options c


selectLast :: (Selection p, Ord a) => Choice p a -> Maybe (Choice p a)
selectLast c = fmap (select1 c) . Set.lookupMax $ options c


fullset :: (Bounded a, Enum a) => Set a
fullset = Set.fromDistinctAscList [minBound..maxBound]


fullOptions :: (Bounded a, Enum a) => Choice 'One a
fullOptions = Choice Nothing fullset


emptyFromList :: Ord a => [a] -> Choice 'One a
emptyFromList xs = emptyFromSet $ Set.fromList xs


emptyFromSet :: Set a -> Choice 'One a
emptyFromSet = Choice Nothing


nonEmptyFromList :: Ord a => [a] -> Maybe (Choice 'AtleastOne a)
nonEmptyFromList xs'@(_:_) = let x = minimum xs' in Just $ Choice x $ Set.fromList xs'
nonEmptyFromList _        = Nothing


fromNonEmpty :: Ord a => NE.NonEmpty a -> Choice 'AtleastOne a
fromNonEmpty xs' = let (x NE.:| xs) = NE.sort xs' in withOptions x $ Set.fromList xs


selectWhen :: Ord a => (a -> Bool) -> Choice p a -> Maybe (Choice 'Many a)
selectWhen p (Choice _ xs) = if sub == Set.empty then Nothing else Just (Choice sub xs)
  where sub = Set.filter p xs


selectFirstWhen :: Ord a => (a -> Bool) -> Choice p a -> Maybe (Choice 'One a)
selectFirstWhen p (Choice _ xs) = if sub == Set.empty then Nothing else selectFirst $ emptyFromSet sub
  where sub = Set.filter p xs


selectLastWhen :: Ord a => (a -> Bool) -> Choice p a -> Maybe (Choice 'One a)
selectLastWhen p (Choice _ xs) = if sub == Set.empty then Nothing else selectLast $ emptyFromSet sub
  where sub = Set.filter p xs


toSet :: Choice p a -> Set a
toSet = options


toList :: Choice p a -> [a]
toList = Set.toList . options


singleton :: a -> Choice 'AtleastOne a
singleton x = Choice x $ Set.singleton x


before :: Ord a => Choice 'AtleastOne a -> Set a
before (Choice x xs) = Set.filter (< x) xs


unsafeSelectAt :: Int -> Choice p a -> Choice 'AtleastOne a
unsafeSelectAt i (Choice _ xs) = Choice (Set.elemAt i xs) xs


getIndex :: Ord a => Choice 'AtleastOne a -> Int
getIndex (Choice x xs) = findIndex x xs


after :: Ord a => Choice 'AtleastOne a -> Set a
after (Choice x xs) = Set.filter (> x) xs


size :: Choice p a -> Int
size (Choice _ xs) = Set.size xs


insert :: Ord a => a -> Choice p a -> Choice p a
insert y (Choice x xs) = Choice x (Set.insert y xs)


delete :: Ord a => a -> Choice 'One a -> Choice 'One a
delete y (Choice x xs) = Choice (if x == Just y then Nothing else x) $ Set.delete y xs


addSelection :: Ord a => a -> Choice 'Many a -> Choice 'Many a
addSelection y c = select c $ Set.singleton y


deselectMany :: Ord a => Set a -> Choice 'Many a -> Maybe (Choice 'Many a)
deselectMany y (Choice x xs) = let w = Set.difference x y in if w == Set.empty then Nothing else Just $ Choice w xs


deselect1 :: Ord a => a -> Choice 'Many a -> Maybe (Choice 'Many a)
deselect1 x = deselectMany (Set.singleton x)


replaceSelection :: (Selection p, Ord a) => Selected p a -> Choice p a -> Choice p a
replaceSelection y c = let Choice _ xs = select c y in Choice y xs


data ConsideredChoice p a = ConsideredChoice
  { consideration :: Considered p a
  , choice        :: Choice p a
  }


deriving instance (Show (Selected p a), Show (Considered p a), Show a)        => Show (ConsideredChoice p a)
deriving instance (Read (Selected p a), Read (Considered p a), Read a, Ord a) => Read (ConsideredChoice p a)
deriving instance (Eq   (Selected p a), Eq   (Considered p a), Eq a)          => Eq   (ConsideredChoice p a)
deriving instance (Ord  (Selected p a), Ord  (Considered p a), Ord a)         => Ord  (ConsideredChoice p a)
deriving instance Generic (ConsideredChoice p a)


type family Considered (p :: Pick) a where
  Considered 'One        a = a
  Considered 'AtleastOne a = a
  Considered 'Many       a = Set a


class InjectConsidered (p :: Pick) where injectConsidered :: Ord a => Considered p a -> Choice p a -> Choice p a
instance InjectConsidered 'One     where injectConsidered = Shpadoinkle.Widgets.Types.Choice.insert
instance InjectConsidered 'Many    where injectConsidered xs (Choice y ys) = Choice y (xs <> ys)


consider :: (InjectConsidered p, Ord a) => Considered p a -> ConsideredChoice p a -> ConsideredChoice p a
consider x (ConsideredChoice _ c) = ConsideredChoice x (injectConsidered x c)


shrug :: ConsideredChoice p a -> Choice p a
shrug = choice


choose :: (Considered p a ~ Selected p a, Selection p, Ord a)
       => ConsideredChoice p a -> ConsideredChoice p a
choose (ConsideredChoice x xs) = ConsideredChoice x $ select xs x
