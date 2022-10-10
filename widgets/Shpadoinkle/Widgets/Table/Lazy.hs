{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wno-type-defaults      #-}


module Shpadoinkle.Widgets.Table.Lazy
  ( AssumedRowHeight (..)
  , AssumedTableHeight (..)
  , CurrentScrollY (..)
  , LazyTabular (..)
  , LazyTable (LazyTable)
  , DebounceScroll
  , LazyTableScrollConfig (..)
  , Offset (..)
  , Length (..)
  , Page (..)
  , RowsToShow (..)
  , RowsLoaded (..)
  , Paginator (..)
  , lazyLoadingTable
  , lazyTable
  ) where


import           Prelude                   hiding (div)

import           Control.Arrow             (second)
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson
import           Data.List                 (sortBy)
import           Data.Proxy
import           Data.Text                 hiding (filter, find, take)
import           GHC.Generics
import           Shpadoinkle.JSFFI         (JSM, JSObject, downcastJSM, getProp,
                                            toNumberLax)

import           Shpadoinkle
import           Shpadoinkle.Html          (div)
import           Shpadoinkle.Widgets.Table
import           Shpadoinkle.Widgets.Types

default (Text)


second3 :: (b -> b') -> (a, b, c) -> (a, b', c)
second3 f (x, y, z) = (x, f y, z)


class Tabular a => LazyTabular a where
  countRows :: a -> Int


data LazyTable a =
  LazyTable
  { tableData   :: a
  , tableHeight :: AssumedTableHeight
  , rowHeight   :: AssumedRowHeight
  , scrollY     :: CurrentScrollY
  , rowsToShow  :: RowsToShow
  , _rowsLoaded :: RowsLoaded
  , paginator   :: Paginator a
  , _sortCol    :: SortCol a
  , rows        :: [Row (LazyTable a)]
  }


newtype RowsToShow = RowsToShow { unRowsToShow :: Int }
  deriving (Eq, Ord, Num, Real, Bounded, Enum, Read, Show, Generic, NFData)

instance ToJSON    RowsToShow
instance FromJSON  RowsToShow


newtype RowsLoaded = RowsLoaded { unRowsLoaded :: Int }
  deriving (Eq, Ord, Num, Real, Bounded, Enum, Read, Show, Generic, NFData)

instance ToJSON    RowsLoaded
instance FromJSON  RowsLoaded


data instance (Row (LazyTable a)) = LazyRow (Row a) | FakeRow


newtype instance (Column (LazyTable a)) = LazyColumn (Column a)


unLazySortCol :: SortCol (LazyTable a) -> SortCol a
unLazySortCol (SortCol (LazyColumn col) ord) = SortCol col ord


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


instance ( Tabular a ) => Tabular (LazyTable a) where
  type Effect (LazyTable a) m = Effect a m
  toRows LazyTable {rows} = rows ++ [FakeRow]
  toCell LazyTable {tableData} (LazyRow r) (LazyColumn c) =
    mapToLazyTable <$> toCell tableData r c
  toCell _ FakeRow _ = []
  sortTable sc (LazyRow a) (LazyRow b) = sortTable (fromLazySortCol sc) a b
  sortTable _ FakeRow FakeRow          = EQ
  sortTable _ _ FakeRow                = LT
  sortTable _ FakeRow _                = GT
  ascendingIcon _ = mapToLazyTableSc $ ascendingIcon Proxy
  descendingIcon _ = mapToLazyTableSc $ descendingIcon Proxy
  handleSort LazyTable {tableData, rowsToShow, paginator} sc = voidRunContinuationT $ do
    tableData' <- lift $ unPaginator paginator tableData (unLazySortCol sc) (Page 0 (Length (unRowsToShow rowsToShow)))
    commit . pur $ \(LazyTable _ th' rh' sy' rts' _ paginator' _ _, sc') ->
                    (LazyTable tableData' th' rh' sy' rts'
                               (RowsLoaded (unRowsToShow rowsToShow))
                               paginator'
                               (unLazySortCol sc')
                               (LazyRow <$> toRows tableData')
                     , sc')


-- Require the user to provide assumptions about the height of each row and the height of the container rather than querying the DOM for this information. Also make the assumption that all rows have equal height.
newtype AssumedRowHeight = AssumedRowHeight Int -- measured in pixels
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Read, Show, Num, Enum, Real, Integral, NFData)


newtype AssumedTableHeight = AssumedTableHeight Int -- measued in pixels
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Read, Show, Num, Enum, Real, Integral, NFData)


type DebounceScroll m a = (RawNode -> RawEvent -> JSM (Continuation m a))
                       -> (RawNode -> RawEvent -> JSM (Continuation m a))


data LazyTableScrollConfig m a b = ContainerIsScrollable (DebounceScroll m (b, CurrentScrollY, RowsLoaded))
                                 | TbodyIsScrollable (DebounceScroll m (LazyTable a, SortCol (LazyTable a)))
  deriving Generic


toLazySortCol :: SortCol a -> SortCol (LazyTable a)
toLazySortCol (SortCol c' s') = SortCol (LazyColumn c') s'


fromLazySortCol :: SortCol (LazyTable a) -> SortCol a
fromLazySortCol (SortCol (LazyColumn c') s') = SortCol c' s'


mapFromLazyTableSc :: Tabular a => Functor m => Continuous f
                   => LazyTable a
                   -> f m (LazyTable a, SortCol (LazyTable a))
                   -> f m ((a, SortCol a), CurrentScrollY, RowsLoaded)
mapFromLazyTableSc (LazyTable _xs tableHeight rowHeight _sy _rts _rl paginator _sc _rs) = liftC
  (\(LazyTable tab _ _ sy _ rl _ _ _, sc') _ -> ((tab, fromLazySortCol sc'), sy, rl))
  (\((tab, sc), sy, rl) -> ( toLazyTable tableHeight rowHeight sy rl paginator tab sc
                           , toLazySortCol sc ))


mapToLazyTable :: forall m a f. Functor m => Continuous f => Tabular a
               => f m a -> f m (LazyTable a)
mapToLazyTable = liftC
  (\tab (LazyTable _ tableHeight rowHeight scrollY _ rowsLoaded paginator sc _)
    -> toLazyTable tableHeight rowHeight scrollY rowsLoaded paginator tab sc)
  (\(LazyTable tab _ _ _ _ _ _ _ _) -> tab)


mapToLazyTableSc :: Functor m => Continuous f => Tabular a
                 => f m (a, SortCol a) -> f m (LazyTable a, SortCol (LazyTable a))
mapToLazyTableSc = liftC
  (\(tab, sc) (LazyTable _ tableHeight rowHeight scrollY _ rowsLoaded paginator _ _, _)
    -> ( toLazyTable tableHeight rowHeight scrollY rowsLoaded paginator tab sc
       , toLazySortCol sc ))
  (\(LazyTable {tableData}, sc) -> (tableData, fromLazySortCol sc))


toLazyTable :: Tabular a
  => AssumedTableHeight -> AssumedRowHeight -> CurrentScrollY
  -> RowsLoaded -> Paginator a -> a -> SortCol a -> LazyTable a
toLazyTable tabh rowh sy rowsLoaded paginator xs sc
  = LazyTable xs tabh rowh sy (RowsToShow numRows) rowsLoaded paginator sc
                 . fmap LazyRow
                 . take numRows
                 . sortBy (sortTable sc)
                 . filter (toFilter xs)
                 $ toRows xs
  where numRows = computeRowsToShow tabh rowh sy


computeRowsToShow :: AssumedTableHeight -> AssumedRowHeight -> CurrentScrollY
           -> Int
computeRowsToShow (AssumedTableHeight height) (AssumedRowHeight rowHeight) (CurrentScrollY scrollY) =
    1 + truncate (pixelsToFill / fromIntegral rowHeight)
  where
    pixelsToFill :: Double
    -- TODO: make these coefficients (8 and 1.5) configurable?
    pixelsToFill = 8 * fromIntegral height + 1.5 * fromIntegral scrollY


-- | A Paginator takes a tabular data type `a` and a sort order and a page and returns an action which yields a new tabular value with the values in the given page range included.
newtype Paginator a = Paginator { unPaginator :: forall m. ( Applicative m, Effect a m ) => a -> SortCol a -> Page -> m a }


-- | A trivialPaginator is a no-op paginator, for when the data is all there already.
trivialPaginator :: Paginator a
trivialPaginator = Paginator (\x _ _ -> pure x)


lazyTable :: forall m a b.
  ( LazyTabular a
  , Monad m
  , Effect a m
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
lazyTable theme tableHeight rowHeight scrollConfig container xs sc scrollY
  = removeRowsLoaded $
    lazyLoadingTable trivialPaginator (RowsLoaded 0) theme tableHeight rowHeight scrollConfig
      liftedContainer xs sc scrollY
  where
    liftedContainer = addRowsLoaded . container . removeRowsLoaded

    addRowsLoaded :: Continuous f => f m (x, y) -> f m (x, y, RowsLoaded)
    addRowsLoaded = liftC (\(x,y) (_,_,r) -> (x,y,r)) (\(x,y,_) -> (x,y))

    removeRowsLoaded :: Continuous f => f m (x, y, RowsLoaded) -> f m (x, y)
    removeRowsLoaded = liftC (\(x,y,_) _ -> (x,y)) (\(x,y) -> (x,y,0))


lazyLoadingTable :: forall m a b.
  ( LazyTabular a
  , Monad m
  , Effect a m
  , Humanize (Column a)
  , Bounded  (Column a)
  , Ord      (Column a)
  , Enum     (Column a) )
  => Paginator a
  -> RowsLoaded
  -> Theme m a
  -> AssumedTableHeight
  -> AssumedRowHeight
  -> LazyTableScrollConfig m a b
  -> (Html m ((a, SortCol a), CurrentScrollY, RowsLoaded)
       -> Html m (b, CurrentScrollY, RowsLoaded))
  -> a
  -> SortCol a
  -> CurrentScrollY
  -> Html m (b, CurrentScrollY, RowsLoaded)
lazyLoadingTable paginator rowsLoaded theme tableHeight rowHeight@(AssumedRowHeight rowHeight')
          scrollConfig container xs sc@(SortCol c s) scrollY =
    addContainerScrollHandler
  . container
  . addContainerFakeHeight
  . mapFromLazyTableSc lazyTab
  $ viewWith lazyTheme lazyTab (SortCol (LazyColumn c) s)
  where
    lazyTab@LazyTable {} = toLazyTable tableHeight rowHeight scrollY rowsLoaded paginator xs sc

    totalRows = countRows xs

    addContainerFakeHeight = case scrollConfig of
      ContainerIsScrollable _ -> div [("style", textProp fakeHeightStyle)] . (:[])
      TbodyIsScrollable _ -> id

    addContainerScrollHandler = case scrollConfig of
      ContainerIsScrollable debounceScroll ->
        mapProps ([listenRaw "scroll" (debounceScroll scrollHandlerContainer)] <>)
      TbodyIsScrollable _ -> id

    scrollHandlerContainer (RawNode n) _ =
      pur . second3 . const . CurrentScrollY . round <$> (toNumberLax =<< getProp ("scrollTop" :: Text) =<< downcastJSM @JSObject n)

    scrollHandlerTbody :: RawNode -> RawEvent -> JSM (Continuation m (LazyTable a, SortCol (LazyTable a)))
    scrollHandlerTbody (RawNode n) _ = do
      sy <- CurrentScrollY . round <$> (toNumberLax =<< getProp "scrollTop" =<< downcastJSM @JSObject n)
      let totalRows' = computeRowsToShow tableHeight rowHeight sy
          offset     = Offset $ unRowsLoaded rowsLoaded
          newRows    = Length $ totalRows' - unRowsLoaded rowsLoaded
      if newRows > 0
      then return . voidRunContinuationT $ do
        xs' <- lift $ unPaginator paginator xs sc (Page offset newRows)
        commit . pur $ \(LazyTable {tableHeight = tableHeight', rowHeight = rowHeight'', paginator = paginator'}, sc') ->
                         (LazyTable xs' tableHeight' rowHeight'' sy
                           (RowsToShow totalRows')
                           (RowsLoaded totalRows')
                           paginator'
                           (unLazySortCol sc')
                           (LazyRow <$> toRows xs')
                         , sc')
      else return . pur $ \(tab, sc') -> (tab { scrollY = sy }, sc')

    fakeHeightStyle =
      "height: " <> pack (show (totalRows * rowHeight')) <> "px;"

    fakeRowHeightStyle totalRows' (RowsToShow rts) =
      "height: " <> pack (show ((totalRows' - rts) * rowHeight')) <> "px;"

    lazyTheme :: Theme m (LazyTable a)
    lazyTheme = case theme of
      Theme tp hp hrp rp thp bp dp -> Theme
        { tableProps = \LazyTable {tableData = xs'} sc' ->
            second mapToLazyTableSc <$> tp xs' (fromLazySortCol sc')
        , headProps = \LazyTable {tableData = xs'} sc' ->
            second mapToLazyTableSc <$> hp xs' (fromLazySortCol sc')
        , htrProps = \LazyTable {tableData = xs'} sc' ->
            second mapToLazyTableSc <$> hrp xs' (fromLazySortCol sc')
        , trProps = \LazyTable {tableData = xs', rowsToShow = rts} sc' r ->
            case r of
              LazyRow r' -> second mapToLazyTableSc <$> rp xs' (fromLazySortCol sc') r'
              FakeRow -> [("style", textProp (fakeRowHeightStyle (countRows xs') rts))]
        , thProps = \LazyTable {tableData = xs'} sc' (LazyColumn c') ->
            second mapToLazyTableSc <$> thp xs' (fromLazySortCol sc') c'
        , bodyProps = \LazyTable {tableData = xs'} sc' ->
            (second mapToLazyTableSc <$> bp xs' (fromLazySortCol sc'))
            ++
            (case scrollConfig of
              ContainerIsScrollable _ -> []
              TbodyIsScrollable debounceScroll -> [ listenRaw "scroll" $ debounceScroll scrollHandlerTbody ])
        , tdProps = \LazyTable {tableData = xs'} sc' r (LazyColumn c') ->
            case r of
              LazyRow r' -> second mapToLazyTable <$> dp xs' (fromLazySortCol sc') r' c'
              FakeRow -> [] }
