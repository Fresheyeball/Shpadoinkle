{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
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


module Shpadoinkle.Widgets.Types.ConsideredChoice where


import           Control.Applicative              (Alternative ((<|>)))
import           Control.Compactable              (Compactable (compact, filter, partition, separate))
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Kind                        (Type)
import           Data.Proxy
import           Data.Set                         as Set
import           GHC.Generics                     (Generic)
#ifdef TESTING
import           Test.QuickCheck                  (Arbitrary (..))
#endif


import           Shpadoinkle.Widgets.Types.Choice


data ConsideredChoice p a = ConsideredChoice
  { _consideration :: Considered p a
  , _choice        :: Choice p a
  }


deriving instance (Show (Selected p a), Show (Considered p a), Show a)        => Show (ConsideredChoice p a)
deriving instance (Read (Selected p a), Read (Considered p a), Read a, Ord a) => Read (ConsideredChoice p a)
deriving instance (Eq   (Selected p a), Eq   (Considered p a), Eq a)          => Eq   (ConsideredChoice p a)
deriving instance (Ord  (Selected p a), Ord  (Considered p a), Ord a)         => Ord  (ConsideredChoice p a)
deriving instance (Foldable (Choice p), Foldable (Considered p))             => Foldable (ConsideredChoice p)
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


class    PickToConsidered (p :: Pick) where pickToConsidered' :: Proxy p -> a -> Considered p a
instance PickToConsidered 'One        where pickToConsidered' _ = Just
instance PickToConsidered 'AtleastOne where pickToConsidered' _ = Just
instance PickToConsidered 'Many       where pickToConsidered' _ = Set.singleton


pickToConsidered :: forall (p :: Pick) a. PickToConsidered p => a -> Considered p a
pickToConsidered = pickToConsidered' (Proxy @p)


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


instance (PickToSelected p, Considered p ~ Maybe, SetLike (ConsideredChoice p), Selection Choice p)
    => Selection ConsideredChoice p where
  select  (ConsideredChoice c xs) x = ConsideredChoice c (select xs x)
  unselected        = unselected . _choice
  selected          = selected . _choice
  withOptions  x xs = ConsideredChoice Nothing (x `withOptions` xs)
  retain  (ConsideredChoice c xs) ys@(ConsideredChoice c' (Choice y ys')) =
    ConsideredChoice (case c of Just x | Set.member x (toSet ys) -> c; _ -> c') (retain xs $ case c' of
      Nothing  -> Choice y ys'
      Just c'' -> Choice y $ Set.insert c'' ys')


instance SetLike (ConsideredChoice 'Many) => Selection ConsideredChoice 'Many where
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


class (Selection f p, PickToConsidered p) => Consideration f (p :: Pick) where
  consider      :: Ord a => Considered p a -> f p a -> f p a
  choose        :: Ord a => f p a -> f p a
  choice        :: Ord a => f p a -> Choice p a
  considered    :: Ord a => f p a -> Considered p a
  shrug         :: Ord a => f p a -> f p a


consider'
  :: forall (f :: Pick -> Type -> Type) p a
   . (Ord a, Consideration f p)
  => a -> f p a -> f p a
consider' = consider @f @p . pickToConsidered @p


instance Consideration ConsideredChoice 'One where
  consider x = ConsideredChoice x . maybe id Shpadoinkle.Widgets.Types.Choice.insert x . _choice
  choose (ConsideredChoice x xs) = ConsideredChoice Nothing $ select xs x
  choice = _choice
  considered = _consideration
  shrug (ConsideredChoice _ xs) = ConsideredChoice Nothing xs

instance Consideration ConsideredChoice 'AtleastOne where
  consider x = ConsideredChoice x . maybe id Shpadoinkle.Widgets.Types.Choice.insert x . _choice
  choose (ConsideredChoice x xs) = ConsideredChoice Nothing (maybe xs (select xs) x)
  choice = _choice
  considered = _consideration
  shrug (ConsideredChoice _ xs) = ConsideredChoice Nothing xs

instance Selection ConsideredChoice 'Many => Consideration ConsideredChoice 'Many where
  consider xs (ConsideredChoice _ (Choice y ys)) = ConsideredChoice xs (Choice y (xs <> ys))
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


#ifdef TESTING
instance (Ord a, Arbitrary a, Arbitrary (Selected p a), Arbitrary (Considered p a)) => Arbitrary (ConsideredChoice p a) where
  arbitrary = ConsideredChoice <$> arbitrary <*> arbitrary
#endif
