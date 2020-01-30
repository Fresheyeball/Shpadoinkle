{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module View where


-- import           Control.Lens              hiding (view)
import           Shpadoinkle
import qualified Shpadoinkle.Html          as H
-- import           Shpadoinkle.Router        (navigate)
import           Shpadoinkle.Widgets.Table as Table
-- import           Shpadoinkle.Widgets.Types

import           Types


start :: Route -> Frontend
start r = Frontend
  { _table =
    [ SpaceCraft 2 0 (Just "thang") 1 AwayTeam Operational
    , SpaceCraft 3 0 (Just "sweet") 2 Scout    Operational
    , SpaceCraft 4 1 (Just "hey")   3 Scout    Inoperable
    ]
  , _sort  = SortCol SKUT ASC
  , _route = r
  }


tableCfg :: TableConfig m a
tableCfg = TableConfig
  [ H.class' "table table-striped table-bordered" ] [] []


-- view :: MonadJSM m => Frontend -> Html m Frontend
view :: Frontend -> Html m Frontend
view _ = text "foo"
-- view fe = case fe ^. route of
--   RList _ -> H.div "container-fluid"
--    [ H.h2_ [ "Space Craft Roster" ]
--    , ($ fe) . set sort <$> Table.viewWith tableCfg (fe ^. table) (fe ^. sort)
--    , H.a [ H.onClick' (fe <$ navigate @SPA (Echo $ Just "plex")) ] [ text "Go to Echo" ]
--    ]
--   RNew -> text "new"
--   RExisting sid -> H.div_ $ present sid
--   Echo t -> H.div_
--     [ maybe (text "Erie silence") text t
--     , H.a [ H.onClick' (fe <$ navigate @SPA (RList $ Input Clean "")) ] [ "Go To Space Craft Roster" ]
--     ]


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
