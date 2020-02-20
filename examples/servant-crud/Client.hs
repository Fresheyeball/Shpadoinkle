{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Monad.Catch
import           Control.Monad.Reader
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
import           Shpadoinkle.Router          (fullPageSPA, withHydration)
import           UnliftIO

import           Types
import           View


newtype App a = App { runApp :: JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)
#ifndef ghcjs_HOST_OS
  deriving (MonadJSM)
#endif


instance MonadUnliftIO App where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = do ctx <- askJSM; return $ UnliftIO $ \(App m) -> runJSM m ctx


runXHR :: ClientM ~> App
#ifdef ghcjs_HOST_OS
runXHR m = App $ either throwM pure =<< runClientM m
#else
runXHR = error "not supported for ghc"
#endif


instance CRUDSpaceCraft App where
  listSpaceCraft       = runXHR listSpaceCraftM
  getSpaceCraft        = runXHR . getSpaceCraftM
  updateSpaceCraft x y = runXHR $ updateSpaceCraftM x y
  createSpaceCraft     = runXHR . createSpaceCraftM
  deleteSpaceCraft     = runXHR . deleteSpaceCraftM


listSpaceCraftM   :: ClientM [SpaceCraft]
getSpaceCraftM    :: SpaceCraftId -> ClientM (Maybe SpaceCraft)
updateSpaceCraftM :: SpaceCraftId -> SpaceCraftUpdate -> ClientM ()
createSpaceCraftM :: SpaceCraftUpdate -> ClientM SpaceCraftId
deleteSpaceCraftM :: SpaceCraftId -> ClientM ()


(listSpaceCraftM :<|> getSpaceCraftM :<|> updateSpaceCraftM :<|> createSpaceCraftM :<|> deleteSpaceCraftM)
  = client (Proxy @ API)


app :: JSM ()
app = fullPageSPA @ SPA runApp runParDiff (withHydration start) view getBody (const . start) routes


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
