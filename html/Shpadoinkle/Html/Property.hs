{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Shpadoinkle.Html.Property where


import           Control.Monad       (msum)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.String         hiding (unwords)
import           Data.Text
import           Prelude             hiding (unwords)

import           Shpadoinkle
import           Shpadoinkle.Html.TH


type TextProperty a = forall m o. ToPropText a => a -> (Text, Prop m o)


class ToPropText a where toPropText :: a -> Text
instance ToPropText Text where toPropText = id
instance ToPropText Int where toPropText = pack . show
instance ToPropText Float where toPropText = pack . show
instance ToPropText Bool where toPropText = \case True -> "true"; False -> "false"


instance IsString a => IsString (Set a) where
  fromString = Set.singleton . fromString


textProperty :: ToPropText a => Text -> a -> (Text, Prop m o)
textProperty k = (,) k . PText . toPropText


className :: Set.Set Text -> (Text, Prop m o)
className = textProperty "className" . unwords . Set.toList


$(msum <$> mapM mkTextProp
  [ "id'", "type'", "rel", "href", "placeholder", "value"
  ])


