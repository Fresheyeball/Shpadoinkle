{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Proxy
import           Servant.API
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Router          (fullPageSPA, withHydration)
import           Shpadoinkle.Router.Client
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


instance CRUDSpaceCraft App where
  listSpaceCraft       = runXHR App listSpaceCraftM
  getSpaceCraft        = runXHR App . getSpaceCraftM
  updateSpaceCraft x y = runXHR App $ updateSpaceCraftM x y
  createSpaceCraft     = runXHR App . createSpaceCraftM
  deleteSpaceCraft     = runXHR App . deleteSpaceCraftM


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
