{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}


module Shpadoinkle.Website.Types.Effects.Example
  ( ExampleEffects
  , module Control.Monad.Reader
  , module Shpadoinkle.Website.Types.Effects.Swan
  ) where


import           Control.Monad.Reader                   (MonadReader)
import           Shpadoinkle.Isreal.Types               (Code)
import           Shpadoinkle.Website.Types.Effects.Swan (Swan (..))
import           Shpadoinkle.Website.Types.Home         (Examples)
import           UnliftIO.STM                           (TVar)


type ExampleEffects m = (MonadReader (Examples (TVar (Maybe Code))) m, Swan m)
