{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}


module Shpadoinkle.Widgets.Types.Choice where


import           Control.Applicative (Alternative ((<|>)))
import           Control.Compactable (Compactable (compact, filter, partition, separate))
import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Foldable       as F
import           Data.Kind           (Type)
import qualified Data.List.NonEmpty  as NE
import           Data.Proxy
import           Data.Set            as Set (Set, delete, difference, elemAt,
                                             empty, filter, findIndex, findMax,
                                             findMin, foldr,
                                             fromDistinctAscList, fromList,
                                             insert, intersection, lookupGT,
                                             lookupLT, lookupMax, lookupMin,
                                             map, member, partition, singleton,
                                             size, toList, valid)
import           GHC.Generics        (Generic)


data Pick = One | AtleastOne | Many


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


ftoSet :: (Ord a, Foldable g) => g a -> Set a
ftoSet = Set.fromList . F.toList


class    PickToSet (p :: Pick) where pickToSet :: Ord a => Selected p a -> Set a
instance PickToSet 'One        where pickToSet = toSet
instance PickToSet 'AtleastOne where pickToSet = Set.singleton
instance PickToSet 'Many       where pickToSet = toSet


class SetLike (f p) => Selection f (p :: Pick) where
  toSelected'   :: Proxy (f p) -> a -> Selected p a
  select        :: Ord a => f p a -> Selected p a -> f p a
  unselected    :: Ord a => f p a -> Set a
  selected      :: Ord a => f p a -> Selected p a
  withOptions   :: (Foldable g, Ord a) => Selected p a -> g a -> f p a
  retain        :: Ord a => f p a -> f p a -> f p a


select' :: forall f (p :: Pick) a. (Selection f p, Ord a) => f p a -> a -> f p a
select' c = select c . (toSelected @f @p)


withOptions' :: forall f (p :: Pick) a g. (Selection f p, Ord a, Foldable g) => a -> g a -> f p a
withOptions' = withOptions . toSelected @f @p


toSelected :: forall f (p :: Pick) a. (Selection f p) => a -> Selected p a
toSelected = toSelected' (Proxy @(f p))


retain' :: (Ord a, Deselection f p, Foldable g) => f p a -> g a -> f p a
retain' xs = retain xs . noselection


instance Selection Choice 'One where
  toSelected' _ = Just
  select (Choice w xs) y = Choice y (toSet w <> toSet y <> xs)
  unselected (Choice x xs) = maybe xs (`Set.delete` xs) x
  selected = _selected
  withOptions x (ftoSet -> xs) = Choice x $ maybe xs (`Set.insert` xs) x
  retain (Choice w _) xs = case w of
    Just x | Set.member x (toSet xs) -> w `withOptions` xs; _ -> xs


instance Selection Choice 'AtleastOne where
  toSelected' _ = id
  select (Choice _ xs) y = Choice y (Set.insert y xs)
  unselected (Choice x xs) = Set.delete x xs
  selected = _selected
  withOptions x (ftoSet -> xs) = Choice x (Set.insert x xs)
  retain (Choice w _) xs = if Set.member w (toSet xs) then w `withOptions` xs else xs


instance Selection Choice 'Many where
  toSelected' _ = Set.singleton
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


data ConsideredChoice p a = ConsideredChoice
  { _consideration :: Considered p a
  , _choice        :: Choice p a
  }


deriving instance (Show (Selected p a), Show (Considered p a), Show a)        => Show (ConsideredChoice p a)
deriving instance (Read (Selected p a), Read (Considered p a), Read a, Ord a) => Read (ConsideredChoice p a)
deriving instance (Eq   (Selected p a), Eq   (Considered p a), Eq a)          => Eq   (ConsideredChoice p a)
deriving instance (Ord  (Selected p a), Ord  (Considered p a), Ord a)         => Ord  (ConsideredChoice p a)
deriving instance Generic (ConsideredChoice p a)
instance (FromJSON a, FromJSON (Considered p a), FromJSON (Selected p a), Ord a) => FromJSON (ConsideredChoice p a)
instance (ToJSON a,   ToJSON (Considered p a),   ToJSON (Selected p a))          => ToJSON   (ConsideredChoice p a)

instance (Compactable (Choice p), Compactable (Considered p)) => Compactable (ConsideredChoice p) where
  compact (ConsideredChoice x xs) = ConsideredChoice (compact x) (compact xs)
  separate (ConsideredChoice x xs) = let (l,r) = separate xs; (l',r') = separate x in (ConsideredChoice l' l, ConsideredChoice r' r)
  filter p (ConsideredChoice x xs) = ConsideredChoice (Control.Compactable.filter p x) $ Control.Compactable.filter p xs
  partition p (ConsideredChoice x xs) = let (l, r) = Control.Compactable.partition p xs; (l',r') = Control.Compactable.partition p x in (ConsideredChoice l' l, ConsideredChoice r' r)

instance (Ord a, Considered p ~ Maybe, Semigroup (Choice p a))
    => Semigroup (ConsideredChoice p a) where
  ConsideredChoice c cc <> ConsideredChoice c' cc' = ConsideredChoice (c <|> c') (cc <> cc')

instance (Ord a, Considered p ~ Maybe, Monoid (Choice p a))
    => Monoid (ConsideredChoice p a) where
      mempty = ConsideredChoice Nothing mempty

instance {-# OVERLAPPING #-} (Semigroup a, Ord a) => Semigroup (ConsideredChoice 'Many a) where
  ConsideredChoice c cc <> ConsideredChoice c' cc' = ConsideredChoice (c <> c') (cc <> cc')

instance {-# OVERLAPPING #-} (Semigroup a, Ord a)
    => Monoid (ConsideredChoice 'Many a) where
      mempty = ConsideredChoice mempty mempty


type family Considered (p :: Pick) :: Type -> Type where
  Considered 'One        = Maybe
  Considered 'AtleastOne = Maybe
  Considered 'Many       = Set


instance (Considered p ~ Maybe, SetLike (Choice p)) => SetLike (ConsideredChoice p) where
  toSet (ConsideredChoice x xs) = toSet xs <> case x of
    Just y -> Set.singleton y
    _      -> mempty
  smap f (ConsideredChoice x xs) = ConsideredChoice (f <$> x) (smap f xs)
  valid (ConsideredChoice _ xs) = Shpadoinkle.Widgets.Types.Choice.valid xs


instance SetLike (ConsideredChoice 'Many) where
  toSet (ConsideredChoice ys xs) = ys <> toSet xs
  smap f (ConsideredChoice ys xs) = ConsideredChoice (smap f ys) (smap f xs)
  valid (ConsideredChoice ys xs) = Set.valid ys && Shpadoinkle.Widgets.Types.Choice.valid xs


instance (Considered p ~ Maybe, SetLike (ConsideredChoice p), Selection Choice p)
    => Selection ConsideredChoice p where
  toSelected' _     = toSelected @Choice @p
  select  (ConsideredChoice c xs) x = ConsideredChoice c (select xs x)
  unselected        = unselected . _choice
  selected          = selected . _choice
  withOptions  x xs = ConsideredChoice Nothing (x `withOptions` xs)
  retain  (ConsideredChoice c xs) ys@(ConsideredChoice c' (Choice y ys')) =
    ConsideredChoice (case c of Just x | Set.member x (toSet ys) -> c; _ -> c') (retain xs $ case c' of
      Nothing  -> Choice y ys'
      Just c'' -> Choice y $ Set.insert c'' ys')


instance SetLike (ConsideredChoice 'Many) => Selection ConsideredChoice 'Many where
  toSelected' _     = toSelected @Choice @'Many
  select  (ConsideredChoice c xs) x = ConsideredChoice c (select xs x)
  unselected        = unselected . _choice
  selected          = selected . _choice
  withOptions  x xs = ConsideredChoice mempty (x `withOptions` xs)
  retain  (ConsideredChoice x xs) ys@(ConsideredChoice y ys')  =
    ConsideredChoice (Set.intersection x (toSet ys) <> y) (retain xs ys')


instance Selection ConsideredChoice 'One => Deselection ConsideredChoice 'One where
  noselection = ConsideredChoice Nothing . noselection
  deselect (ConsideredChoice c xs) = ConsideredChoice c $ deselect (select xs c)

instance Selection ConsideredChoice 'Many => Deselection ConsideredChoice 'Many where
  noselection = ConsideredChoice mempty . noselection
  deselect (ConsideredChoice c xs) = ConsideredChoice c $ deselect (select xs c)


class Selection f p => Consideration f (p :: Pick) where
  consider   :: Ord a => Considered p a -> f p a -> f p a
  consider'  :: Ord a => a -> f p a -> f p a
  choose     :: Ord a => f p a -> f p a
  choice     :: Ord a => f p a -> Choice p a
  considered :: Ord a => f p a -> Considered p a
  shrug      :: Ord a => f p a -> f p a

instance Consideration ConsideredChoice 'One where
  consider x = ConsideredChoice x . maybe id Shpadoinkle.Widgets.Types.Choice.insert x . _choice
  consider' = consider @ConsideredChoice @'One . Just
  choose (ConsideredChoice x xs) = ConsideredChoice Nothing $ select xs x
  choice = _choice
  considered = _consideration
  shrug (ConsideredChoice _ xs) = ConsideredChoice Nothing xs

instance Consideration ConsideredChoice 'AtleastOne where
  consider x = ConsideredChoice x . maybe id Shpadoinkle.Widgets.Types.Choice.insert x . _choice
  consider' = consider @ConsideredChoice @'AtleastOne . Just
  choose (ConsideredChoice x xs) = ConsideredChoice Nothing (maybe xs (select xs) x)
  choice = _choice
  considered = _consideration
  shrug (ConsideredChoice _ xs) = ConsideredChoice Nothing xs

instance Selection ConsideredChoice 'Many => Consideration ConsideredChoice 'Many where
  consider xs (ConsideredChoice _ (Choice y ys)) = ConsideredChoice xs (Choice y (xs <> ys))
  consider' = consider @ConsideredChoice @'Many . Set.singleton
  choose (ConsideredChoice s xs) = ConsideredChoice Set.empty $ select xs s
  choice = _choice
  considered = _consideration
  shrug (ConsideredChoice _ xs) = ConsideredChoice mempty xs


unsafeConsiderFirst :: (Consideration f p, Ord a) => f p a -> f p a
unsafeConsiderFirst c = Set.findMin (toSet c) `consider'` c


unsafeConsiderLast :: (Consideration f p, Ord a) => f p a -> f p a
unsafeConsiderLast c = Set.findMax (toSet c) `consider'` c


considerNext, considerPrev :: (Considered p a ~ Maybe a, Consideration f p, Ord a) => f p a -> f p a
considerNext c = maybe (unsafeConsiderFirst c) (`consider'` c) $ considered c >>= (\x -> Set.lookupGT x $ toSet c)
considerPrev c = maybe (unsafeConsiderLast c)  (`consider'` c) $ considered c >>= (\x -> Set.lookupLT x $ toSet c)
