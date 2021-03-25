{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Control.Monad.Reader
import           Data.Proxy                         (Proxy (..))
import           Data.Text                          (Text)
import           Servant.API                        (type (:<|>) ((:<|>)))
#ifndef ghcjs_HOST_OS
import           Servant.Server                     as Servant (serve)
#endif

#ifndef ghcjs_HOST_OS
import           Shpadoinkle                        (JSM, MonadJSM,
                                                     MonadUnliftIO (..), TVar,
                                                     constUpdate, liftJSM,
                                                     newTVarIO)
#else
import           Shpadoinkle                        (JSM, MonadUnliftIO (..),
                                                     TVar, constUpdate, liftJSM,
                                                     newTVarIO)
#endif
import           Shpadoinkle.Backend.Snabbdom       (runSnabbdom, stage)
import           Shpadoinkle.Isreal.Types           as Swan (API, Code,
                                                             CompileError,
                                                             SnowNonce,
                                                             SnowToken (..))
import           Shpadoinkle.Router                 (fullPageSPA',
                                                     withHydration)
import           Shpadoinkle.Router.Client          (BaseUrl (..),
                                                     ClientEnv (..), ClientM,
                                                     Scheme (Https), client,
                                                     runXHR')
import           Shpadoinkle.Widgets.Types          (Search (..))
#ifndef ghcjs_HOST_OS
import           Shpadoinkle.Router.Server          (serveUI)
import           Shpadoinkle.Run                    (Env (Dev), liveWithBackend,
                                                     runJSorWarp)
#else
import           Shpadoinkle.Run                    (runJSorWarp)
#endif

import           Shpadoinkle.DeveloperTools
import           Shpadoinkle.Marketing.Types        (Frontend, Hooglable (..),
                                                     HoogleAPI,
                                                     Route (FourOhFourR), SPA,
                                                     Swan (..), routes)
import           Shpadoinkle.Marketing.Types.Hoogle (Target)
#ifndef ghcjs_HOST_OS
import           Shpadoinkle.Marketing.View         (start, startJS, template,
                                                     view)
#else
import           Shpadoinkle.Marketing.View         (start, startJS, view)
#endif


newtype App a = App { runApp :: ReaderT (TVar (Maybe Code)) JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader (TVar (Maybe Code)))
#ifndef ghcjs_HOST_OS
  deriving (MonadJSM)
#endif


instance MonadUnliftIO App where
  withRunInIO inner = App $ ReaderT $ \r ->
    withRunInIO $ \run -> inner (run . flip runReaderT r . runApp)


isrealEnv, hoogleEnv :: ClientEnv
isrealEnv = ClientEnv $ BaseUrl Https "isreal.shpadoinkle.org" 443 ""
hoogleEnv = ClientEnv $ BaseUrl Https "hoogle.shpadoinkle.org" 443 ""


instance Swan App where
  compile t n c = liftJSM $ runXHR' (compileM t n c) isrealEnv
  clean   t     = liftJSM $ runXHR' (cleanM   t)     isrealEnv


instance Hooglable App where
  findTargets s = liftJSM $ runXHR' (findTargetsM s) hoogleEnv


compileM :: SnowToken -> SnowNonce -> Code -> ClientM (Either CompileError Text)
cleanM   :: SnowToken -> ClientM Text
(_ :<|> compileM :<|> cleanM :<|> _)
  = client (Proxy @ Swan.API)


findTargetsM :: Search -> ClientM [Target]
findTargetsM (Search s) = client (Proxy @ HoogleAPI) (Just "json") (Just s) (Just 1) (Just 7)


app :: JSM ()
app = do
  mutex <- newTVarIO Nothing
  model :: TVar Frontend <- newTVarIO =<< runReaderT (runApp (start FourOhFourR)) mutex
  withDeveloperTools model
  fullPageSPA' @(SPA JSM) (flip runReaderT mutex . runApp) runSnabbdom model (withHydration startJS) view stage
    (fmap constUpdate . startJS) routes


main :: IO ()
main = runJSorWarp 8080 app


#ifndef ghcjs_HOST_OS

dev :: IO ()
dev = liveWithBackend 8080 app . pure $ Servant.serve (Proxy @ (SPA IO)) $ serveUI @ (SPA IO) "." (\r -> do
  x <- start r
  return $ template @App Dev x (view x)) routes

#endif
