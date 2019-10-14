{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


module Shpadoinkle.Widgets.Table
  ( Sort (..)
  , SortCol (..)
  , compareOn
  , negateSort
  , TableTerritory (..)
  , toggleSort
  , simple
  ) where


import           Data.Kind
import           Data.List                   (sortBy)
import           Language.Javascript.JSaddle

import           Shpadoinkle
import           Shpadoinkle.Html            hiding (a, a', max, min, s, s')
import qualified Shpadoinkle.Html            as Html
import           Shpadoinkle.Widgets.Types


data Sort = ASC | DESC
  deriving (Show, Eq, Ord, Bounded, Enum)


instance Semigroup Sort where (<>) = min
instance Monoid Sort where mempty = maxBound


negateSort :: Sort -> Sort
negateSort ASC  = DESC
negateSort DESC = ASC


data SortCol a = SortCol (TableColumn a) Sort
deriving instance Show (TableColumn a) => Show (SortCol a)
deriving instance Eq   (TableColumn a) => Eq   (SortCol a)
deriving instance Ord  (TableColumn a) => Ord  (SortCol a)


instance Ord (TableColumn a) => Semigroup (SortCol a) where
  SortCol a s <> SortCol a' s' = SortCol (max a a') (min s s')


instance ( Bounded (TableColumn a)
         , Ord (TableColumn a)
         , Enum (TableColumn a)
         ) => Monoid (SortCol a) where
  mempty = SortCol minBound maxBound


compareOn :: Ord a => Sort -> a -> a -> Ordering
compareOn DESC = compare
compareOn ASC  = flip compare


class TableTerritory a where
  data TableColumn a :: Type
  data TableRow a :: Type
  toRows :: a -> [TableRow a]
  toCells :: TableRow a -> [Html m b]
  sortTable :: SortCol a -> TableRow a -> TableRow a -> Ordering


toggleSort :: Eq (TableColumn a) => TableColumn a -> SortCol a -> SortCol a
toggleSort c (SortCol c' s) = if c == c' then SortCol c $ negateSort s else SortCol c mempty


simple ::
  ( MonadJSM m
  , TableTerritory a
  , Humanize (TableColumn a)
  , Bounded  (TableColumn a)
  , Ord      (TableColumn a)
  , Enum     (TableColumn a) )
  => a -> SortCol a -> Html m (SortCol a)
simple xs s@(SortCol sorton sortorder) =
  table []
  [ thead_ [ tr_ $ cth_ <$> [minBound..maxBound] ]
  , tbody_ $ tr_ . toCells <$> sortBy (sortTable s) (toRows xs)
  ]

  where

  cth_ c = th [] . pure . Html.a [ onClick (toggleSort c s) ]
         . mappend [ text (humanize c) ] . pure . text $
          if c == sorton then
            case sortorder of ASC -> "↑"; DESC -> "↓"
          else ""
