{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


module Shpadoinkle.Widgets.Table
  ( Sort (..)
  , SortCol (..)
  , compareOn
  , negateSort
  , TableTerritory (..)
  , TableConfig (..)
  , toggleSort
  , view
  , viewWith
  ) where


import           Data.Aeson
import           Data.Kind
import           Data.List                   (sortBy)
import           Data.Text
import           GHC.Generics
import           Language.Javascript.JSaddle

import           Shpadoinkle
import           Shpadoinkle.Html            hiding (a, a', max, min, s, s')
import qualified Shpadoinkle.Html            as Html
import           Shpadoinkle.Widgets.Types


data Sort = ASC | DESC
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, ToJSON, FromJSON)


instance Semigroup Sort where (<>) = min
instance Monoid Sort where mempty = maxBound


negateSort :: Sort -> Sort
negateSort ASC  = DESC
negateSort DESC = ASC


data SortCol a = SortCol (TableColumn a) Sort
deriving instance Show (TableColumn a) => Show (SortCol a)
deriving instance Eq   (TableColumn a) => Eq   (SortCol a)
deriving instance Ord  (TableColumn a) => Ord  (SortCol a)
deriving instance Functor TableColumn => Functor SortCol
deriving instance Generic (SortCol a)
instance (ToJSON   (TableColumn a)) => ToJSON (SortCol a)
instance (FromJSON (TableColumn a)) => FromJSON (SortCol a)


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
  data TableRow a    :: Type
  toRows    :: a -> [TableRow a]
  toCell    :: MonadJSM m => TableRow a -> TableColumn a -> [Html m ()]
  sortTable :: SortCol a -> TableRow a -> TableRow a -> Ordering


toggleSort :: Eq (TableColumn a) => TableColumn a -> SortCol a -> SortCol a
toggleSort c (SortCol c' s) = if c == c' then SortCol c $ negateSort s else SortCol c mempty


data TableConfig m a = TableConfig
  { tableProps :: [(Text, Prop m (SortCol a))]
  , headProps  :: [(Text, Prop m (SortCol a))]
  , bodyProps  :: [(Text, Prop m (SortCol a))]
  } deriving Generic


deriving instance Eq   (Prop m (SortCol a)) => Eq   (TableConfig m a)
deriving instance Ord  (Prop m (SortCol a)) => Ord  (TableConfig m a)
deriving instance Show (Prop m (SortCol a)) => Show (TableConfig m a)
instance Semigroup (TableConfig m a) where
  TableConfig x y z <> TableConfig x' y' z' =
    TableConfig (x <> x') (y <> y') (z <> z')
instance Monoid (TableConfig m a) where
  mempty = TableConfig mempty mempty mempty
deriving instance (Functor TableColumn, Functor m) => Functor (TableConfig m)


view :: forall m a.
  ( MonadJSM m
  , TableTerritory a
  , Humanize (TableColumn a)
  , Bounded  (TableColumn a)
  , Ord      (TableColumn a)
  , Enum     (TableColumn a) )
  => a -> SortCol a -> Html m (SortCol a)
view = viewWith mempty


viewWith :: forall m a.
  ( MonadJSM m
  , TableTerritory a
  , Humanize (TableColumn a)
  , Bounded  (TableColumn a)
  , Ord      (TableColumn a)
  , Enum     (TableColumn a) )
  => TableConfig m a -> a -> SortCol a -> Html m (SortCol a)
viewWith TableConfig {..} xs s@(SortCol sorton sortorder) =
  table tableProps
  [ thead headProps [ tr_ $ cth_ <$> [minBound..maxBound] ]
  , tbody bodyProps $ do
      row <- sortBy (sortTable s) (toRows xs)
      return . (s <$) . tr_ $ td_ . toCell row <$> [minBound..maxBound]
  ]

  where

  cth_ c = th [] . pure . Html.a [ onClick (toggleSort c s) ]
         . mappend [ text (humanize c) ] . pure . text $
          if c == sorton then
            case sortorder of ASC -> "↑"; DESC -> "↓"
          else ""
