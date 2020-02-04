{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Lens                (set)
import           Control.Monad.Reader
import           Data.Proxy
import           Data.Set                    (Set)
import           Servant.API
#ifdef ghcjs_HOST_OS
import           Servant.Client.Ghcjs
#else
import           Servant.Client
#endif
import           GHCJS.DOM
import           GHCJS.DOM.Location
import           GHCJS.DOM.Types
import           GHCJS.DOM.Window
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Router          (fullPageSPA)

import           Types
import           View


newtype App a = App { runApp :: ReaderT ClientEnv JSM a }
  deriving (Functor, Applicative, Monad)


getEnv :: JSM ClientEnv
getEnv = do
    curLoc <- getLocation =<< currentWindowUnchecked

    jsStr_protocol <- getProtocol curLoc
    jsStr_port     <- getPort     curLoc
    jsStr_hostname <- getHostname curLoc

    let protocol
          | jsStr_protocol == "https:" = Https
          | otherwise                  = Http

        portStr :: String
        portStr = fromJSString jsStr_port

        port :: Int
        port | null portStr = case protocol of
                 Http  ->  80
                 Https -> 443
             | otherwise = read portStr

        hostname :: String
        hostname = fromJSString jsStr_hostname

    return . ClientEnv $ BaseUrl protocol hostname port ""


runXHR c = do
  env <- ask
  liftJSM $ runClientM env c


instance CRUDSpaceCraft App where
  listSpaceCraft       = App $ runClientM listSpaceCraftM
  getSpaceCraft        = App . getSpaceCraftM
  updateSpaceCraft x y = App $ updateSpaceCraftM x y
  createSpaceCraft     = App . newSpaceCraftM
  deleteSpaceCraft     = App . deleteSpaceCraftM


(listSpaceCraftM :<|> getSpaceCraftM :<|> updateSpaceCraftM :<|> newSpaceCraftM :<|> deleteSpaceCraftM)
  = client (Proxy @API)


app :: JSM ()
app = fullPageSPA @SPA id runParDiff start view getBody (\r fe -> start r) routes


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
