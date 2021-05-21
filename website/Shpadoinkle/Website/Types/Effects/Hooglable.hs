{-# LANGUAGE FlexibleInstances #-}


module Shpadoinkle.Website.Types.Effects.Hooglable where


import           Control.Monad.Trans                     (MonadTrans, lift)
import           Shpadoinkle.Website.Types.Hoogle.Target (Target)
import           Shpadoinkle.Widgets.Types               (Search)


class Hooglable m where
  findTargets :: Search -> m [Target]


instance (MonadTrans t, Monad m, Hooglable m) => Hooglable (t m) where
  findTargets = lift . findTargets
