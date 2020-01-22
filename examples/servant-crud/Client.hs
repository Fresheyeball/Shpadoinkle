{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Lens                hiding (view)
import           Servant.API
#ifdef ghcjs_HOST_OS
import           Servant.Client.Ghcjs
#else
import           Servant.Client
#endif
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import qualified Shpadoinkle.Html            as H
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Router          (fullPageSPA, navigate)
import           Shpadoinkle.Widgets.Table   as Table

import           Types


getSpaceCraft    :: SpaceCraftId -> ClientM SpaceCraft
updateSpaceCraft :: SpaceCraftId -> SpaceCraftUpdate -> ClientM ()
newSpaceCraft    :: SpaceCraftUpdate -> ClientM SpaceCraftId
deleteSpaceCraft :: SpaceCraftId -> ClientM ()


(getSpaceCraft :<|> updateSpaceCraft :<|> newSpaceCraft :<|> deleteSpaceCraft)
  = client api


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
    , H.a [ H.onClick' (fe <$ navigate @SPA Root) ] [ text "ROOT" ]
    ]


app :: JSM ()
app = do
  addStyle "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
  fullPageSPA @SPA id runParDiff start view getBody (const return) routes


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
