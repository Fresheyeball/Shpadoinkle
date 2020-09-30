{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Main where


import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Reader        (MonadIO)
import           Data.Proxy                  (Proxy (..))
import           Language.Javascript.JSaddle (askJSM, runJSM)
import           Servant.API                 ((:<|>) (..))
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html.Utils      (getBody)
import           Shpadoinkle.Router          (fullPageSPA, withHydration)
#ifndef ghcjs_HOST_OS
import           Shpadoinkle.Router          (MonadJSM)
#endif
import           Shpadoinkle.Router.Client   (client, runXHR)
import           UnliftIO                    (MonadUnliftIO (..), UnliftIO (..))

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
  listSpaceCraft       = App $ runXHR listSpaceCraftM
  getSpaceCraft x      = App . runXHR $ getSpaceCraftM x
  updateSpaceCraft x y = App . runXHR $ updateSpaceCraftM x y
  createSpaceCraft x   = App . runXHR $ createSpaceCraftM x
  deleteSpaceCraft x   = App . runXHR $ deleteSpaceCraftM x


(listSpaceCraftM :<|> getSpaceCraftM :<|> updateSpaceCraftM :<|> createSpaceCraftM :<|> deleteSpaceCraftM)
  = client (Proxy @ API)


app :: JSM ()
app = fullPageSPA @ SPA runApp runParDiff (withHydration start) view getBody start routes


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
