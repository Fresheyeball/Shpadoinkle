{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}


module Shpadoinkle.Widgets.Types.Physical where


import           Data.Aeson
import           Data.Functor.Identity
import           GHC.Generics

import           Shpadoinkle.Html               hiding (s)
import           Shpadoinkle.Widgets.Types.Core


data Toggle = Closed Hygiene | Open
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)


data Hover = MouseOver | MouseOut
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, ToJSON, FromJSON)


instance Semigroup Hover where
  MouseOver <> _ = MouseOver
  _ <> MouseOver = MouseOver
  _ <> _         = MouseOut


instance Monoid Hover where
  mempty = MouseOut


withHover
  :: ((Hover, a) -> Html m (Hover, a))
  ->  (Hover, a) -> Html m (Hover, a)
withHover f = runIdentity . props
  (Identity . mappend [ onMouseenter $ (MouseOver, ) . snd
                      , onMouseleave $ (MouseOut,  ) . snd
                      ]) . f


togHygiene :: Toggle -> Hygiene
togHygiene = \case
  Closed x -> x
  Open     -> Dirty


instance Enum Toggle where
  fromEnum (Closed Clean) = 0
  fromEnum Open           = 1
  fromEnum (Closed Dirty) = 2
  toEnum 0 = Closed Clean
  toEnum 1 = Open
  toEnum 2 = Closed Dirty
  toEnum _ = error "Not a valid Toggle"


instance Bounded Toggle where
  minBound = Closed Clean
  maxBound = Closed Dirty


instance Semigroup Toggle where
  Closed x <> Closed y = Closed (x <> y)
  Closed Clean <> x    = x
  x <> Closed Clean    = x
  Closed Dirty <> _    = Closed Dirty
  _ <> Closed Dirty    = Closed Dirty
  _ <> _               = Open


instance Monoid Toggle where
  mempty = Closed Clean


class IsToggle a where
  close    :: a -> a
  toggle   :: a -> a
  open     :: a -> a


instance IsToggle Toggle where

  close = \case
    Open -> Closed Dirty
    t    -> t

  toggle = \case
    Open     -> Closed Dirty
    Closed _ -> Open

  open = const Open


data Visbility = Visible | Hidden
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
