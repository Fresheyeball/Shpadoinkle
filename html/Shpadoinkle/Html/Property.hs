{-# OPTIONS_GHC -fno-warn-orphans #-}


module Shpadoinkle.Html.Property
  ( TextProperty
  , ToPropText(..)
  , className
  , id'
  , type'
  , rel
  , href
  , style
  ) where


import qualified Data.Set          as Set

import           Shpadoinkle.Class


type TextProperty a = forall m o. ToPropText a => a -> (Text, Prop m o)


class ToPropText a where toPropText :: a -> Text
instance ToPropText Text where toPropText = id
instance ToPropText Int where toPropText = present
instance ToPropText Bool where toPropText = \case True -> "true"; False -> "false"


instance IsString a => IsString (Set a) where
  fromString = Set.singleton . fromString


textProperty :: ToPropText a => Text -> a -> (Text, Prop m o)
textProperty k = (k, ) . PText . toPropText


className :: Set.Set Text -> (Text, Prop m o)
className = textProperty "className" . unwords . Set.toList


id' :: Text -> (Text, Prop m o)
id' = textProperty "id"


type' :: Text -> (Text, Prop m o)
type' = textProperty "type"


rel :: Text -> (Text, Prop m o)
rel = textProperty "rel"


href :: Text -> (Text, Prop m o)
href = textProperty "href"


style :: Text -> (Text, Prop m o)
style = textProperty "style"
