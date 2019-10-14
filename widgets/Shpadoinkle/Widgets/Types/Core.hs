{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Shpadoinkle.Widgets.Types.Core where


import           Data.Text
import           Language.Javascript.JSaddle

import           Shpadoinkle


data Hygiene = Clean | Dirty
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


instance Semigroup Hygiene where
  Clean <> Clean = Clean
  _ <> _ = Dirty


instance Monoid Hygiene where
  mempty = Clean


data Ability = Enabled | Disabled
  deriving (Eq, Ord, Show, Enum, Bounded)


toBool :: Ability -> Bool
toBool Enabled  = True
toBool Disabled = False


fromBool :: Bool -> Ability
fromBool True  = Enabled
fromBool False = Disabled


class Humanize a where humanize :: a -> Text

class Present a where present :: MonadJSM m => a -> [Html m b]

instance {-# OVERLAPPABLE #-} Humanize a => Present a where
  present = pure . text . humanize
  {-# INLINE present #-}
