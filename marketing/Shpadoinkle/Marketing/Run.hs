{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Control.Monad.Catch          (MonadThrow)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Proxy                   (Proxy (..))
import           Data.Text                    (Text, pack)
import           Data.Time                    (getCurrentTime)
import           Servant.API                  (type (:<|>) ((:<|>)))
import           System.Random                (Random (randomRIO))
#ifndef ghcjs_HOST_OS
import           Servant.Server               (serve)
#endif

#ifndef ghcjs_HOST_OS
import           Shpadoinkle                  (JSM, MonadJSM,
                                               MonadUnliftIO (..),
                                               UnliftIO (..), askJSM, runJSM)
#else
import           Shpadoinkle                  (JSM, MonadUnliftIO (..),
                                               UnliftIO (..), askJSM, runJSM)
#endif
import           Shpadoinkle.Backend.Snabbdom (runSnabbdom, stage)
import           Shpadoinkle.Isreal.Types     as Swan (API, Code, CompileError,
                                                       SnowToken (..))
import           Shpadoinkle.Router           (fullPageSPA, withHydration)
import           Shpadoinkle.Router.Client    (ClientM, client, runXHR)
#ifndef ghcjs_HOST_OS
import           Shpadoinkle.Router.Server    (serveUI)
import           Shpadoinkle.Run              (Env (Dev), liveWithBackend,
                                               runJSorWarp)
#else
import           Shpadoinkle.Run              (runJSorWarp)
#endif

import           Shpadoinkle.Marketing.Types  (SPA, Swan (..), routes)
#ifndef ghcjs_HOST_OS
import           Shpadoinkle.Marketing.View   (start, template, view)
#else
import           Shpadoinkle.Marketing.View   (start, view)
#endif


newtype App a = App { runApp :: JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)
#ifndef ghcjs_HOST_OS
  deriving (MonadJSM)
#endif


instance MonadUnliftIO App where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = do ctx <- askJSM; return $ UnliftIO $ \(App m) -> runJSM m ctx


instance Swan App where
  token       = App . liftIO $ do
    (cur, rand) <- (,) <$> getCurrentTime <*> randomRIO (1, 1000000 :: Double)
    return . SnowToken $ pack (show cur) <> "-" <> pack (show rand)
  compile t c = App . runXHR $ compileM t c
  clean   t   = App . runXHR $ cleanM   t


compileM :: SnowToken -> Code -> ClientM (Either CompileError Text)
cleanM   :: SnowToken -> ClientM Text
(_ :<|> compileM :<|> cleanM :<|> _)
  = client (Proxy @ Swan.API)


app :: JSM ()
app = fullPageSPA @ (SPA JSM) runApp runSnabbdom (withHydration start) view stage start routes


main :: IO ()
main = runJSorWarp 8080 app


#ifndef ghcjs_HOST_OS

dev :: IO ()
dev = liveWithBackend 8080 app . pure $ serve (Proxy @ (SPA IO)) $ serveUI @ (SPA IO) "./static" (\r -> do
  x <- start r
  return $ template @IO Dev x (view x)) routes

#endif
