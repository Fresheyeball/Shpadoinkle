{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Control.Monad.Catch                         (MonadThrow)
import           Control.Monad.IO.Class                      (MonadIO (..))
import           Control.Monad.Reader
import           Data.Proxy                                  (Proxy (..))
import           Data.Text                                   (Text)
import           Servant.API                                 (type (:<|>) ((:<|>)))
#ifndef ghcjs_HOST_OS
import           Servant.Server                              as Servant (serve)
#endif

#ifndef ghcjs_HOST_OS
import           Shpadoinkle                                 (JSM, MonadJSM,
                                                              MonadUnliftIO (..),
                                                              TVar, constUpdate,
                                                              liftJSM,
                                                              newTVarIO)
#else
import           Shpadoinkle                                 (JSM,
                                                              MonadUnliftIO (..),
                                                              TVar, constUpdate,
                                                              liftJSM,
                                                              newTVarIO)
#endif
import           Shpadoinkle.Backend.Snabbdom                (runSnabbdom,
                                                              stage)
import           Shpadoinkle.Isreal.Types                    as Swan (API, Code,
                                                                      CompileError,
                                                                      SnowNonce,
                                                                      SnowToken (..))
import           Shpadoinkle.Router                          (fullPageSPA',
                                                              withHydration)
import           Shpadoinkle.Router.Client                   (BaseUrl (..),
                                                              ClientEnv (..),
                                                              ClientM,
                                                              Scheme (Https),
                                                              client, runXHR',
                                                              runXHRe)
import           Shpadoinkle.Widgets.Types                   (Search (..))
#ifndef ghcjs_HOST_OS
import           Shpadoinkle.Router.Server                   (serveUI)
import           Shpadoinkle.Run                             (Env (Dev),
                                                              liveWithBackend,
                                                              runJSorWarp)
#else
import           Shpadoinkle.Run                             (runJSorWarp)
#endif

-- import           Shpadoinkle.DeveloperTools
import           Shpadoinkle.Website.Types.CurrentYear       (getCurrentYear)
import           Shpadoinkle.Website.Types.Effects.Hooglable (Hooglable (..))
import           Shpadoinkle.Website.Types.Effects.Swan      (Swan (..))
import           Shpadoinkle.Website.Types.Home              (Examples (..))
import           Shpadoinkle.Website.Types.Hoogle.API        as Hoogle
import           Shpadoinkle.Website.Types.Hoogle.Target     (Target)
import           Shpadoinkle.Website.Types.Route             (Route (RFourOhFour))
import           Shpadoinkle.Website.Types.SPA               (SPA, routes)
#ifndef ghcjs_HOST_OS
import           Shpadoinkle.Website.Partials.Template       (template)
import           Shpadoinkle.Website.View                    (start, startJS,
                                                              view)
#else
import           Shpadoinkle.Website.View                    (start, startJS,
                                                              view)
#endif


newtype App a = App { runApp :: ReaderT (Examples (TVar (Maybe Code))) JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader (Examples (TVar (Maybe Code))))
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
  compile t n c = do
    res <- liftJSM $ runXHRe (compileM t n c) isrealEnv
    case res of
      Left _  -> compile t n c
      Right x -> pure x
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
  mutex <- Examples <$> newTVarIO Nothing <*> newTVarIO Nothing <*> newTVarIO Nothing
  model <- newTVarIO =<< runReaderT (runApp (start RFourOhFour)) mutex
  yc <- getCurrentYear
  fullPageSPA' @(SPA JSM)
    (flip runReaderT mutex . runApp)
    runSnabbdom
    model
    (withHydration (startJS model))
    (view yc)
    stage
    (fmap constUpdate . startJS model) routes


main :: IO ()
main = runJSorWarp 8080 app


#ifndef ghcjs_HOST_OS

dev :: IO ()
dev = liveWithBackend 8080 app . pure $ Servant.serve (Proxy @ (SPA IO)) $ serveUI @(SPA IO) "." (\r -> do
  vm <- start r
  cy <- getCurrentYear
  return $ template @App Dev vm (view cy vm)) routes

#endif
