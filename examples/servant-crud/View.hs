{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module View where


import           Control.Lens              hiding (view)
import           Shpadoinkle
import qualified Shpadoinkle.Html          as H
import           Shpadoinkle.Router        (navigate)
import           Shpadoinkle.Widgets.Table as Table
import           Shpadoinkle.Widgets.Types

import           Types


toEditForm :: SpaceCraft -> EditForm
toEditForm sc = EditForm
  { _sku         = Input Clean $ sc ^. sku
  , _description = Input Clean $ sc ^. description
  , _serial      = Input Clean $ sc ^. serial
  , _squadron    = (sc ^. squadron) `withOptions'` fullset
  , _operable    = (sc ^. operable) `withOptions'` fullset
  }


start :: (Monad m, CRUDSpaceCraft m) => Route -> m Frontend
start = \case
  RList s -> MList . Roster (SortCol SKUT ASC) s <$> listSpaceCraft
  REcho t -> return $ MEcho t
  RNew -> return $ MDetail Nothing $ emptyEditForm
  RExisting i -> do
    mcraft <- getSpaceCraft i
    return $ case mcraft of
     Just craft -> MDetail (Just i) $ toEditForm craft
     _          -> M404


tableCfg :: TableConfig m a
tableCfg = TableConfig
  [ H.class' "table table-striped table-bordered" ] [] []


(<+) :: Functor m => s -> ASetter' s a -> Html m a -> Html m s
(<+) s out = fmap $ \a -> s & out .~ a


(<+-) :: Functor m => s -> ASetter' s b -> (a -> Html m b) -> Getting a s a -> Html m s
(<+-) big out trans in0 = (\s -> set out s big) <$> trans (big ^. in0)


(<+--) :: Functor m => s -> ASetter' s c -> (a -> b -> Html m c) -> Getting a s a -> Getting b s b -> Html m s
(<+--) big out trans in0 in1 = (\s -> set out s big) <$> trans (big ^. in0) (big ^. in1)


(<+---) :: Functor m => s -> ASetter' s d -> (a -> b -> c -> Html m d) -> Getting a s a -> Getting b s b -> Getting c s c -> Html m s
(<+---) big out trans in0 in1 in2 = (\s -> set out s big) <$> trans (big ^. in0) (big ^. in1) (big ^. in2)


view :: MonadJSM m => Frontend -> Html m Frontend
view fe = case fe of
  MList r -> MList <$> H.div "container-fluid"
   [ H.h2_ [ "Space Craft Roster" ]
   , (r <+-- sort) (Table.viewWith tableCfg) table sort
   , H.a [ H.onClick' (r <$ navigate @SPA (REcho $ Just "plex")) ] [ text "Go to Echo" ]
   ]
  MDetail sid _ -> H.div_ $ maybe [text "New"] present sid
  MEcho t -> H.div_
    [ maybe (text "Erie silence") text t
    , H.a [ H.onClick' (fe <$ navigate @SPA (RList $ Input Clean "")) ] [ "Go To Space Craft Roster" ]
    ]
  M404 -> text "404"


template :: Html m a -> Html m a
template stage = H.html_
  [ H.head_
    [ H.link'
      [ H.rel "stylesheet"
      , H.href "https://cdn.usebootstrap.com/bootstrap/4.3.1/css/bootstrap.min.css"
      ]
    , H.meta [ H.charset "ISO-8859-1" ] []
    , H.script [ H.src "/all.js" ] []
    ]
  , H.body_
    [ stage
    ]
  ]
