{-# LANGUAGE FlexibleInstances #-}


module Shpadoinkle.Website.Types.Effects.Swan where


import           Control.Monad.Trans      (MonadTrans (lift))
import           Data.Text                (Text)
import           Shpadoinkle.Isreal.Types (Code, CompileError, SnowNonce,
                                           SnowToken)


class Swan m where
  compile :: SnowToken -> SnowNonce -> Code -> m (Either CompileError Text)
  clean   :: SnowToken -> m Text


instance (MonadTrans t, Monad m, Swan m) => Swan (t m) where
  compile x y = lift . compile x y
  clean       = lift . clean
