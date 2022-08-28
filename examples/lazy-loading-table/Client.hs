{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}


module Main where


import           Prelude                        hiding (div, init, span)

import           Control.Arrow                  (first)
import           Data.CountryCodes
import           Data.Proxy
import qualified Data.Set                       as Set
import           Data.Text                      hiding (init, reverse, span)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html               hiding (a, b, head, max)
import           Shpadoinkle.Router.Client      (BaseUrl (..), ClientEnv (..),
                                                 ClientM, Scheme (Http), client,
                                                 runXHR')
import           Shpadoinkle.Run                (run)
import           Shpadoinkle.Widgets.Table
import           Shpadoinkle.Widgets.Table.Lazy

import           Types

default (Text)


initialPage :: Page
initialPage = Page 0 400


filterView :: MonadJSM m => Model -> Html m Model
filterView m =
  div_ [
    div_ [
      text "Filter by sex: ",
      span [ onClickC (setSexFilter (Just Male)) ] [ "Male" ],
      text " / ",
      span [ onClickC (setSexFilter (Just Female)) ] [ "Female" ],
      text " / ",
      span [ onClickC (setSexFilter Nothing) ] [ "Either" ]
    ],
    div_ [
      text "Filter by country of origin:",
      div_ $ originWidget m <$> allNames
    ]
  ]


setSexFilter :: MonadJSM m => Maybe Sex -> Continuation m Model
setSexFilter f = voidRunContinuationT $ do
  commit . pur $ \(tab, sc) -> (tab { filters = (filters tab) { bySex = f } }, sc)
  commit resetData

originWidget :: MonadJSM m => Model -> (CountryCode, Text) -> Html m Model
originWidget (tab, _sc) (cc, cName) = div_ [
  input' [ ("type", "checkbox")
         , checked $ Set.member cc (byOrigin (filters tab))
         , onClickC (toggleOriginFilter cc) ],
  text cName ]


toggleOriginFilter :: MonadJSM m => CountryCode -> Continuation m Model
toggleOriginFilter cc = voidRunContinuationT $ do
  commit . pur $ toggleOriginFilterPure cc
  commit resetData


resetData :: MonadJSM m => Continuation m Model
resetData = kleisli $ \(tab, sc) -> do
  tab' <- loadMore tab sc initialPage
  return . pur . first $ const tab'


toggleOriginFilterPure :: CountryCode -> Model -> Model
toggleOriginFilterPure cc (tab, sc) =
  if Set.member cc (byOrigin (filters tab))
  then ( tab { filters = (filters tab) { byOrigin = Set.delete cc (byOrigin (filters tab)) } }
       , sc )
  else ( tab { filters = (filters tab) { byOrigin = Set.insert cc (byOrigin (filters tab)) } }
       , sc )


getPersonsM :: Page -> SortCol FilteredTable -> TableFilters -> ClientM [Person]
getPersonsM = client (Proxy :: Proxy Api)


getPersons :: MonadJSM m => Page -> SortCol FilteredTable -> TableFilters -> m [Person]
getPersons pg sc fs =
  liftJSM $
  runXHR'
  (getPersonsM pg sc fs)
  (ClientEnv (BaseUrl Http "localhost" 8081 ""))


first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f (a,b,c) = (f a, b, c)


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a


loadMore :: MonadJSM m => FilteredTable -> SortCol FilteredTable -> Page -> m FilteredTable
loadMore tab sc page@(Page (Offset off) _)
  = (\newRows -> tab { contents = Prelude.take off (contents tab) ++ newRows })
     <$> getPersons page sc (filters tab)


mainView :: MonadJSM m
         => DebounceScroll m (LazyTable FilteredTable, SortCol (LazyTable FilteredTable))
         -> (Model, CurrentScrollY, RowsLoaded)
         -> Html m (Model, CurrentScrollY, RowsLoaded)
mainView debounceScroll (m@(tab, sc), sy, rl) = div_ [
    lazyLoadingTable (Paginator loadMore) rl theme (AssumedTableHeight 500) (AssumedRowHeight 20) (TbodyIsScrollable debounceScroll) id tab sc sy,
    liftC (first3 . const) fst3 $ filterView m
  ]
  where

    theme :: Theme m FilteredTable
    theme = mempty { bodyProps = const $ const [("style", "display: block; overflow: auto; height: 500px;")]
                   , headProps = const $ const [("style", "display: block;")] }


main :: IO ()
main = do
  ds <- debounceRaw 0.25
  let init = ((FilteredTable [] (TableFilters Nothing Set.empty), SortCol Name ASC), CurrentScrollY 0, RowsLoaded 0)
  model <- newTVarIO init
  run $ do
    atomically . modifyTVar model . first3 =<< runContinuation resetData (fst3 init)
    shpadoinkle id runParDiff model (mainView ds) getBody
