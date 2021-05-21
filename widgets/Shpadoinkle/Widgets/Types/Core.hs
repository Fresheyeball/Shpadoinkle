{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Shpadoinkle.Widgets.Types.Core where


import           Data.Aeson
import           Data.Text
import           GHC.Generics
#ifdef TESTING
import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum)
#endif

import           Shpadoinkle


data Hygiene = Clean | Dirty
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, ToJSON, FromJSON, NFData)


data ClickAway = ClosesOnClickAway | StaysOpenOnClickAway
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, ToJSON, FromJSON, NFData)


instance Semigroup Hygiene where
  Clean <> Clean = Clean
  _ <> _         = Dirty


instance Monoid Hygiene where
  mempty = Clean


data Ability = Enabled | Disabled
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, ToJSON, FromJSON, NFData)


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


instance Humanize () where
  humanize _ = pack "∅"


instance Humanize Text where
  humanize = id
  {-# INLINE humanize #-}


instance Humanize String where
  humanize = pack
  {-# INLINE humanize #-}


class Present a where
  present :: a -> [Html m b]
  default present :: Humanize a => a -> [Html m b]
  present = pure . text . humanize
  {-# INLINE present #-}


instance {-# OVERLAPPABLE #-} Humanize a => Present a


#ifdef TESTING
instance Arbitrary Hygiene where arbitrary = arbitraryBoundedEnum
#endif
