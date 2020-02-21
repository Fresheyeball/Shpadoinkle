{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}


module Shpadoinkle.Widgets.Table
  ( Sort (..)
  , SortCol (..)
  , compareOn
  , negateSort
  , Tabular (..)
  , Column, Row
  , Config (..)
  , toggleSort
  , view
  , viewWith
  ) where


import           Data.Aeson
import           Data.Kind
import           Data.List                 (sortBy)
import           Data.Text
import           GHC.Generics

import           Shpadoinkle
import           Shpadoinkle.Html          hiding (a, a', max, min, s, s')
import qualified Shpadoinkle.Html          as Html
import           Shpadoinkle.Widgets.Types


data Sort = ASC | DESC
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, ToJSON, FromJSON)


instance Semigroup Sort where (<>) = min
instance Monoid Sort where mempty = maxBound


negateSort :: Sort -> Sort
negateSort ASC  = DESC
negateSort DESC = ASC


data SortCol a = SortCol (Column a) Sort
deriving instance Show (Column a) => Show (SortCol a)
deriving instance Eq   (Column a) => Eq   (SortCol a)
deriving instance Ord  (Column a) => Ord  (SortCol a)
deriving instance Functor Column => Functor SortCol
deriving instance Generic (SortCol a)
instance (ToJSON   (Column a)) => ToJSON   (SortCol a)
instance (FromJSON (Column a)) => FromJSON (SortCol a)


instance Ord (Column a) => Semigroup (SortCol a) where
  SortCol a s <> SortCol a' s' = SortCol (max a a') (min s s')


instance ( Bounded (Column a)
         , Ord (Column a)
         , Enum (Column a)
         ) => Monoid (SortCol a) where
  mempty = SortCol minBound maxBound


compareOn :: Ord a => Sort -> a -> a -> Ordering
compareOn DESC = compare
compareOn ASC  = flip compare


data family Column (a :: Type) :: Type
data family Row    (a :: Type) :: Type


class Tabular a where
  type Effect a (m :: Type -> Type) :: Constraint
  type Effect a m = Applicative m
  toRows    :: a -> [Row a]
  toCell    :: Effect a m => a -> Row a -> Column a -> [Html m a]
  sortTable :: SortCol a -> Row a -> Row a -> Ordering


toggleSort :: Eq (Column a) => Column a -> SortCol a -> SortCol a
toggleSort c (SortCol c' s) = if c == c' then SortCol c $ negateSort s else SortCol c mempty


data Config m a = Config
  { tableProps :: [(Text, Prop m (a ,SortCol a))]
  , headProps  :: [(Text, Prop m (a, SortCol a))]
  , bodyProps  :: [(Text, Prop m (a, SortCol a))]
  } deriving Generic


deriving instance Eq   (Prop m (a, SortCol a)) => Eq   (Config m a)
deriving instance Ord  (Prop m (a, SortCol a)) => Ord  (Config m a)
deriving instance Show (Prop m (a, SortCol a)) => Show (Config m a)
instance Semigroup (Config m a) where
  Config x y z <> Config x' y' z' =
    Config (x <> x') (y <> y') (z <> z')
instance Monoid (Config m a) where
  mempty = Config mempty mempty mempty
deriving instance (Functor Column, Functor m) => Functor (Config m)


view :: forall m a.
  ( Tabular a
  , Effect a m
  , Applicative m
  , Humanize (Column a)
  , Bounded  (Column a)
  , Ord      (Column a)
  , Enum     (Column a) )
  => a -> SortCol a -> Html m (a, SortCol a)
view = viewWith mempty


viewWith :: forall m a.
  ( Tabular a
  , Effect a m
  , Applicative m
  , Humanize (Column a)
  , Bounded  (Column a)
  , Ord      (Column a)
  , Enum     (Column a) )
  => Config m a -> a -> SortCol a -> Html m (a, SortCol a)
viewWith Config {..} xs s@(SortCol sorton sortorder) =
  table tableProps
    [ thead headProps [ tr_ $ cth_ <$> [minBound..maxBound] ]
    , tbody bodyProps $ do
        row <- sortBy (sortTable s) (toRows xs)
        return . (fmap (, s)) . tr_ $ td_ . toCell xs row <$> [minBound..maxBound]
    ]

  where

  cth_ c = th [] . pure . Html.a [ onClick (xs, toggleSort c s) ]
         . mappend [ text (humanize c) ] . pure . text $
          if c == sorton then
            case sortorder of ASC -> "↑"; DESC -> "↓"
          else ""
