{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Client where


import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Reader        (MonadIO)
import           Data.Proxy                  (Proxy (..))
#ifndef ghcjs_HOST_OS
import           Shpadoinkle                 (JSM, MonadUnliftIO (..),
                                              UnliftIO (..), askJSM, runJSM)
#else
import           Shpadoinkle                 (JSM, MonadUnliftIO (..),
                                              UnliftIO (..), askJSM, runJSM)
#endif
import           Servant.API                 ((:<|>) (..))
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html.Utils      (getBody)
import           Shpadoinkle.Router          (fullPageSPA, withHydration)
import           Shpadoinkle.Router.Client   (client, runXHR)

import           Types                       (API, CRUDSpaceCraft (..), SPA,
                                              routes)
import           View                        (start, view)


newtype App a = App { runApp :: JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)


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
app = fullPageSPA @ (SPA JSM) runApp runParDiff (withHydration start) view getBody start routes
