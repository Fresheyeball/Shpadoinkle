{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Lens                (set)
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
import           Shpadoinkle.Router          (fullPageSPA)

import           Types
import           View

import           Debug.Trace


getSpaceCraft    :: SpaceCraftId -> ClientM SpaceCraft
updateSpaceCraft :: SpaceCraftId -> SpaceCraftUpdate -> ClientM ()
newSpaceCraft    :: SpaceCraftUpdate -> ClientM SpaceCraftId
deleteSpaceCraft :: SpaceCraftId -> ClientM ()


(getSpaceCraft :<|> updateSpaceCraft :<|> newSpaceCraft :<|> deleteSpaceCraft)
  = client (Proxy @API)


tracy :: Show a => String -> a -> a
tracy s x = trace (s ++ show x) x


app :: JSM ()
app = fullPageSPA @SPA id runParDiff start view getBody (fmap return . set route . tracy "route") routes


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
