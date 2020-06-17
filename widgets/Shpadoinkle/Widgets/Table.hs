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
  , Theme (..)
  , toggleSort
  , view
  , viewWith
  ) where


import           Control.Arrow             (second)
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
  toCell    :: Effect a m => a -> Row a -> Column a -> [HtmlM m a]
  sortTable :: SortCol a -> Row a -> Row a -> Ordering


toggleSort :: Eq (Column a) => Column a -> SortCol a -> SortCol a
toggleSort c (SortCol c' s) = if c == c' then SortCol c $ negateSort s else SortCol c mempty


data Theme m a = Theme
  { tableProps ::             [(Text, PropM m (a, SortCol a))]
  , headProps  ::             [(Text, PropM m (a, SortCol a))]
  , thProps    :: Column a -> [(Text, PropM m (a, SortCol a))]
  , bodyProps  ::             [(Text, PropM m (a, SortCol a))]
  , tdProps    :: Column a -> [(Text, PropM m a)]
  } deriving Generic


instance Semigroup (Theme m a) where
  Theme v w x y z <> Theme v' w' x' y' z' =
    Theme (v <> v') (w <> w') (x <> x') (y <> y') (z <> z')
instance Monoid (Theme m a) where
  mempty = Theme mempty mempty mempty mempty mempty


view :: forall m a.
  ( Tabular a
  , Effect a m
  , Monad m
  , Humanize (Column a)
  , Bounded  (Column a)
  , Ord      (Column a)
  , Enum     (Column a) )
  => a -> SortCol a -> HtmlM m (a, SortCol a)
view = viewWith mempty


viewWith :: forall m a.
  ( Tabular a
  , Effect a m
  , Monad m
  , Humanize (Column a)
  , Bounded  (Column a)
  , Ord      (Column a)
  , Enum     (Column a) )
  => Theme m a -> a -> SortCol a -> HtmlM m (a, SortCol a)
viewWith Theme {..} xs s@(SortCol sorton sortorder) =
  table tableProps
    [ thead headProps [ tr_ $ cth_ <$> [minBound..maxBound] ]
    , tbody bodyProps $ do
        row <- sortBy (sortTable s) (toRows xs)
        return . leftMC . tr_ $ (\c -> td (tdProps c) $ toCell xs row c) <$> [minBound..maxBound]
    ]

  where

  cth_ c = th (thProps c) . pure . Html.a [ second rightMC . onClick $ toggleSort c s ]
         . mappend [ text (humanize c) ] . pure . text $
          if c == sorton then
            case sortorder of ASC -> "↑"; DESC -> "↓"
          else ""
