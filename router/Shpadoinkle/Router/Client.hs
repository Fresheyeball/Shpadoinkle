{-# LANGUAGE CPP           #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Helper for querying the server from client side code, using a derived client.
-- This module exists to save you from having to use CPP yourself.

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
import           Language.Javascript.JSaddle
import           UnliftIO


-- | Run the ClientM from Servant as an XHR request.
-- Raises an exception if evalued with GHC.
runXHR :: MonadIO m => MonadThrow m => (JSM a -> m a) -> ClientM a -> m a
#ifdef ghcjs_HOST_OS
runXHR f m = f $ either throwM pure =<< runClientM m
#else
runXHR = error "not supported for ghc"
#endif
