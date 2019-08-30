{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Shpadoinkle.Widgets.Types.Core where


data Hygiene = Clean | Dirty
  deriving (Eq, Ord, Show, Enum, Bounded)


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



