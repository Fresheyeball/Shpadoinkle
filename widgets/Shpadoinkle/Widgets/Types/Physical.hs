module Shpadoinkle.Widgets.Types.Physical where


import           Shpadoinkle.Widgets.Types.Core


data Toggle = Closed Hygiene | Open
  deriving (Eq, Ord, Show)


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
  Closed Clean <> x = x
  x <> Closed Clean = x
  Closed Dirty <> _ = Closed Dirty
  _ <> Closed Dirty = Closed Dirty
  _ <> _ = Open


instance Monoid Toggle where
  mempty = Closed Clean


setClosed :: Toggle -> Toggle
setClosed t = case t of
  Open -> Closed Dirty
  _    -> t


data Visbility = Visible | Hidden
  deriving (Eq, Ord, Show, Enum, Bounded)