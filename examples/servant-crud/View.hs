{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module View where


import           Control.Lens              hiding (view)
import           Shpadoinkle
import qualified Shpadoinkle.Html          as H
import           Shpadoinkle.Router        (navigate)
import           Shpadoinkle.Widgets.Table as Table

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


view :: MonadJSM m => Frontend -> Html m Frontend
view fe = case fe ^. route of
  Root   -> H.div_
   [ ($ fe) . set sort <$> Table.simple (fe ^. table) (fe ^. sort)
   , H.a [ H.onClick' (fe <$ navigate @SPA (Echo $ Just "plex")) ] [ text "ECHO" ]
   ]
  Echo t -> H.div_
    [ maybe (text "Erie silence") text t
    , H.a [ H.onClick' (fe <$ navigate @SPA Root) ] [ text " - Echo" ]
    ]


template :: Html m a -> Html m a
template stage = H.html_
  [ H.head_
    [ H.style'
      [ H.rel "stylesheet"
      , H.src "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
      ]
    ]
  , H.body_
    [ stage
    ]
  ]
