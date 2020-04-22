{-# LANGUAGE CPP           #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Shpadoinkle.Router.Client
  ( runXHR
#ifdef ghcjs_HOST_OS
  , module Servant.Client.Ghcjs
#else
  , module Servant.Client
#endif
  ) where


import           Control.Monad.Catch
#ifdef ghcjs_HOST_OS
import           Servant.Client.Ghcjs
#else
import           Servant.Client
#endif
import           UnliftIO

import           Shpadoinkle


runXHR :: MonadIO m => MonadThrow m => (JSM a -> m a) -> ClientM a -> m a
#ifdef ghcjs_HOST_OS
runXHR f m = f $ either throwM pure =<< runClientM m
#else
runXHR = error "not supported for ghc"
#endif
