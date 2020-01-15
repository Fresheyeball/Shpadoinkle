{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Shpadoinkle.Widgets.Types.Core where


import           Data.Text

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


class Humanize a where
  humanize :: a -> Text
  default humanize :: Show a => a -> Text
  humanize = pack . show
  {-# INLINE humanize #-}


class Present a where
  present :: a -> [Html m b]
  default present :: Humanize a => a -> [Html m b]
  present = pure . text . humanize
  {-# INLINE present #-}


instance {-# OVERLAPPABLE #-} Humanize a => Present a
