{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-type-defaults      #-}


module Shpadoinkle.Widgets.Table.Lazy
  ( AssumedRowHeight (..)
  , AssumedTableHeight (..)
  , CurrentScrollY (..)
  , LazyTabular (..)
  , LazyTable (..)
  , DebounceScroll
  , LazyTableScrollConfig (..)
  , lazyTable
  ) where


import           Prelude                     hiding (div)

import           Control.Arrow               (second)
import           Data.Aeson
import           Data.List                   (sortBy)
import           Data.Maybe                  (fromMaybe)
import           Data.Proxy
import           Data.Text                   hiding (filter, find, take)
import           GHC.Generics

import           Language.Javascript.JSaddle hiding (JSM, MonadJSM)
import           Shpadoinkle
import           Shpadoinkle.Html            (div)
import           Shpadoinkle.Widgets.Table
import           Shpadoinkle.Widgets.Types

default (Text)


class Tabular a => LazyTabular a where
  countRows :: a -> Int


data LazyTable a = LazyTable a AssumedTableHeight AssumedRowHeight CurrentScrollY RowsToShow (SortCol a) [Row (LazyTable a)]


newtype RowsToShow = RowsToShow Int
  deriving (Eq, Ord, Num, Real, Bounded, Enum, Read, Show, ToJSON, FromJSON, Generic, NFData)


data instance (Row (LazyTable a)) = LazyRow (Row a) | FakeRow


newtype instance (Column (LazyTable a)) = LazyColumn (Column a)


instance Humanize (Column a) => Humanize (Column (LazyTable a)) where
  humanize (LazyColumn c) = humanize c


instance Bounded (Column a) => Bounded (Column (LazyTable a)) where
  minBound = LazyColumn minBound
  maxBound = LazyColumn maxBound


instance Eq (Column a) => Eq (Column (LazyTable a)) where
  (LazyColumn a) == (LazyColumn b) = a == b


instance Enum (Column a) => Enum (Column (LazyTable a)) where
  toEnum = LazyColumn . toEnum
  fromEnum (LazyColumn c) = fromEnum c


instance Ord (Column a) => Ord (Column (LazyTable a)) where
  compare (LazyColumn a) (LazyColumn b) = compare a b


instance Tabular a => Tabular (LazyTable a) where
  type Effect (LazyTable a) m = Effect a m
  toRows (LazyTable _ _ _ _ _ _ rows) = rows ++ [FakeRow]
  toCell (LazyTable xs _ _ _ _ _ _) (LazyRow r) (LazyColumn c) =
    mapToLazyTable <$> toCell xs r c
  toCell _ FakeRow _ = []
  sortTable sc (LazyRow a) (LazyRow b) = sortTable (fromLazySortCol sc) a b
  sortTable _ FakeRow FakeRow          = EQ
  sortTable _ _ FakeRow                = LT
  sortTable _ FakeRow _                = GT
  ascendingIcon _ = mapToLazyTableSc $ ascendingIcon Proxy
  descendingIcon _ = mapToLazyTableSc $ descendingIcon Proxy


-- Require the user to provide assumptions about the height of each row and the height of the container rather than querying the DOM for this information. Also make the assumption that all rows have equal height.
newtype AssumedRowHeight = AssumedRowHeight Int -- measured in pixels
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Read, Show, Num, Enum, Real, Integral, NFData)


newtype AssumedTableHeight = AssumedTableHeight Int -- measued in pixels
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Read, Show, Num, Enum, Real, Integral, NFData)


newtype CurrentScrollY = CurrentScrollY Int -- measured in pixels
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Read, Show, Num, Enum, Real, Integral, NFData)


type DebounceScroll m a = (RawNode -> RawEvent -> JSM (Continuation m a))
                       -> (RawNode -> RawEvent -> JSM (Continuation m a))


data LazyTableScrollConfig m a b = ContainerIsScrollable (DebounceScroll m (b, CurrentScrollY))
                                 | TbodyIsScrollable (DebounceScroll m (LazyTable a, SortCol (LazyTable a)))
  deriving Generic


toLazySortCol :: SortCol a -> SortCol (LazyTable a)
toLazySortCol (SortCol c' s') = SortCol (LazyColumn c') s'


fromLazySortCol :: SortCol (LazyTable a) -> SortCol a
fromLazySortCol (SortCol (LazyColumn c') s') = SortCol c' s'


mapFromLazyTableSc :: Tabular a => Functor m => Continuous f
                 => LazyTable a
                 -> f m (LazyTable a, SortCol (LazyTable a)) -> f m ((a, SortCol a), CurrentScrollY)
mapFromLazyTableSc (LazyTable _ tableHeight rowHeight _ _ _ _) = liftC
  (\(LazyTable tab _ _ sy _ _ _, sc') _ -> ((tab, fromLazySortCol sc'), sy))
  (\((tab, sc), sy) -> ( toLazyTable tableHeight rowHeight sy tab sc
                       , toLazySortCol sc ))


mapToLazyTable :: Functor m => Continuous f => Tabular a
               => f m a -> f m (LazyTable a)
mapToLazyTable = liftC
  (\tab (LazyTable _ tableHeight rowHeight scrollY _ sc _)
    -> toLazyTable tableHeight rowHeight scrollY tab sc)
  (\(LazyTable tab _ _ _ _ _ _) -> tab)


mapToLazyTableSc :: Functor m => Continuous f => Tabular a
                 => f m (a, SortCol a) -> f m (LazyTable a, SortCol (LazyTable a))
mapToLazyTableSc = liftC
  (\(tab, sc) (LazyTable _ tableHeight rowHeight scrollY _ _ _, _)
    -> ( toLazyTable tableHeight rowHeight scrollY tab sc
       , toLazySortCol sc ))
  (\(LazyTable tab _ _ _ _ _ _, sc) -> (tab, fromLazySortCol sc))


toLazyTable :: Tabular a
  => AssumedTableHeight -> AssumedRowHeight -> CurrentScrollY
  -> a -> SortCol a -> LazyTable a
toLazyTable tabh@(AssumedTableHeight height) rowh@(AssumedRowHeight rowHeight) sy@(CurrentScrollY scrollY) xs sc
  = LazyTable xs tabh rowh sy (RowsToShow rowsToShow) sc
                 . fmap LazyRow
                 . take rowsToShow
                 . sortBy (sortTable sc)
                 . filter (toFilter xs)
                 $ toRows xs
  where
    pixelsToFill :: Double
    -- TODO: make these coefficients (8 and 1.5) configurable?
    pixelsToFill = 8 * fromIntegral height + 1.5 * fromIntegral scrollY

    rowsToShow :: Int = 1 + truncate (pixelsToFill / fromIntegral rowHeight)


lazyTable :: forall m a b.
  ( LazyTabular a
  , Effect a m
  , MonadJSM m
  , Humanize (Column a)
  , Bounded  (Column a)
  , Ord      (Column a)
  , Enum     (Column a) )
  => Theme m a
  -> AssumedTableHeight
  -> AssumedRowHeight
  -> LazyTableScrollConfig m a b
  -> (Html m ((a, SortCol a), CurrentScrollY) -> Html m (b, CurrentScrollY))
  -> a
  -> SortCol a
  -> CurrentScrollY
  -> Html m (b, CurrentScrollY)
lazyTable theme tableHeight rowHeight@(AssumedRowHeight rowHeight')
          scrollConfig container xs sc@(SortCol c s) scrollY =
    addContainerScrollHandler
  . container
  . addContainerFakeHeight
  . mapFromLazyTableSc lazyTab
  $ viewWith lazyTheme lazyTab (SortCol (LazyColumn c) s)
  where
    lazyTab@LazyTable {} = toLazyTable tableHeight rowHeight scrollY xs sc

    totalRows = countRows xs

    addContainerFakeHeight = case scrollConfig of
      ContainerIsScrollable _ -> div [("style", textProp fakeHeightStyle)] . (:[])
      TbodyIsScrollable _ -> id

    addContainerScrollHandler = case scrollConfig of
      ContainerIsScrollable debounceScroll ->
        mapProps ([listenRaw "scroll" (debounceScroll scrollHandlerContainer)] <>)
      TbodyIsScrollable _ -> id

    scrollHandlerContainer (RawNode n) _ =
      pur . second . const . CurrentScrollY . fromMaybe 0
        <$> (fromJSVal =<< n ! "scrollTop")

    scrollHandlerTbody :: RawNode -> RawEvent -> JSM (Continuation m (LazyTable a, SortCol (LazyTable a)))
    scrollHandlerTbody (RawNode n) _ = do
      sy <- CurrentScrollY . fromMaybe 0 <$> (fromJSVal =<< n ! "scrollTop")
      return . pur $ \(LazyTable t th rh _ rts sc' rs, sc'') -> (LazyTable t th rh sy rts sc' rs, sc'')

    fakeHeightStyle =
      "height: " <> pack (show (totalRows * rowHeight')) <> "px;"

    fakeRowHeightStyle totalRows' (RowsToShow rts) =
      "height: " <> pack (show ((totalRows' - rts) * rowHeight')) <> "px;"

    lazyTheme :: Theme m (LazyTable a)
    lazyTheme = case theme of
      Theme tp hp hrp rp thp bp dp -> Theme
        { tableProps = \(LazyTable xs' _ _ _ _ _ _) sc' ->
            second mapToLazyTableSc <$> tp xs' (fromLazySortCol sc')
        , headProps = \(LazyTable xs' _ _ _ _ _ _) sc' ->
            second mapToLazyTableSc <$> hp xs' (fromLazySortCol sc')
        , htrProps = \(LazyTable xs' _ _ _ _ _ _) sc' ->
            second mapToLazyTableSc <$> hrp xs' (fromLazySortCol sc')
        , trProps = \(LazyTable xs' _ _ _ rts _ _) sc' r ->
            case r of
              LazyRow r' -> second mapToLazyTableSc <$> rp xs' (fromLazySortCol sc') r'
              FakeRow -> [("style", textProp (fakeRowHeightStyle (countRows xs') rts))]
        , thProps = \(LazyTable xs' _ _ _ _ _ _) sc' (LazyColumn c') ->
            second mapToLazyTableSc <$> thp xs' (fromLazySortCol sc') c'
        , bodyProps = \(LazyTable xs' _ _ _ _ _ _) sc' ->
            (second mapToLazyTableSc <$> bp xs' (fromLazySortCol sc'))
            ++
            (case scrollConfig of
              ContainerIsScrollable _ -> []
              TbodyIsScrollable debounceScroll -> [ listenRaw "scroll" $ debounceScroll scrollHandlerTbody ])
        , tdProps = \(LazyTable xs' _ _ _ _ _ _) sc' r (LazyColumn c') ->
            case r of
              LazyRow r' -> second mapToLazyTable <$> dp xs' (fromLazySortCol sc') r' c'
              FakeRow -> [] }
