{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Lens                hiding (view)
import           Data.Proxy
import           Servant.API
#ifdef ghcjs_HOST_OS
import           Servant.Client.Ghcjs
#else
import           Servant.Client
#endif
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Widgets.Table   as Table

import           Types


getSpaceCraft    :: SpaceCraftId -> ClientM SpaceCraft
updateSpaceCraft :: SpaceCraftId -> SpaceCraftUpdate -> ClientM ()
newSpaceCraft    :: SpaceCraftUpdate -> ClientM SpaceCraftId
deleteSpaceCraft :: SpaceCraftId -> ClientM ()

(getSpaceCraft :<|> updateSpaceCraft :<|> newSpaceCraft :<|> deleteSpaceCraft)
  = client (Proxy @API)


start :: Frontend
start = Frontend
  { _table =
    [ SpaceCraft 2 0 (Just "thang") 1 AwayTeam Operational
    , SpaceCraft 3 0 (Just "sweet") 2 Scout    Operational
    , SpaceCraft 4 1 (Just "hey")   3 Scout    Inoperable
    ]
  , _sort  = SortCol SKUT ASC
  }


view :: MonadJSM m => Frontend -> Html m Frontend
view fe = ($ fe) . set sort <$> Table.simple (fe ^. table) (fe ^. sort)


app :: JSM ()
app = do
  addStyle "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
  fullPage runParDiff start view getBody


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
