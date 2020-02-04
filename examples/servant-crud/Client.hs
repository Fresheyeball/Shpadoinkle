{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
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
import           Data.Aeson
import           Data.ByteString.Lazy        (fromStrict)
import           Data.Proxy
import           Data.Set
import           Data.Text.Encoding
import           Servant.API
#ifdef ghcjs_HOST_OS
import           Servant.Client.Ghcjs
#else
import           Servant.Client
#endif
import           GHCJS.DOM.Types             (askJSM, runJSM)
import           Language.Javascript.JSaddle (fromJSVal, jsg)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Router          (fullPageSPA)
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
  askUnliftIO = do
    ctx <- askJSM
    return $ UnliftIO $ \(App m) -> runJSM m ctx


runXHR :: ClientM a -> App a
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


listSpaceCraftM   :: ClientM (Set SpaceCraft)
getSpaceCraftM    :: SpaceCraftId -> ClientM (Maybe SpaceCraft)
updateSpaceCraftM :: SpaceCraftId -> SpaceCraftUpdate -> ClientM ()
createSpaceCraftM :: SpaceCraftUpdate -> ClientM SpaceCraftId
deleteSpaceCraftM :: SpaceCraftId -> ClientM ()


(listSpaceCraftM :<|> getSpaceCraftM :<|> updateSpaceCraftM :<|> createSpaceCraftM :<|> deleteSpaceCraftM)
  = client (Proxy @ API)


startWithContext :: Route -> App Frontend
startWithContext r = do
  i <- liftJSM $ fromJSVal =<< jsg "initState"
  case decode . fromStrict . encodeUtf8 =<< i of
    Just fe -> return fe
    _       -> start r



app :: JSM ()
app = fullPageSPA @ SPA runApp runParDiff startWithContext view getBody (const . start) routes


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
