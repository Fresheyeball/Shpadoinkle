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


class' :: Text -> (Text, Prop m o)
class' = className . Set.singleton


autofocus :: Bool -> (Text, Prop m o)
autofocus b = ("autofocus", flag b)


for' :: Text -> (Text, Prop m o)
for' = textProperty "htmlFor"


checked :: Bool -> (Text, Prop m o)
checked b = ("checked", flag b)


hidden :: Bool -> (Text, Prop m o)
hidden b = ("hidden", flag b)


selected :: Bool -> (Text, Prop m o)
selected b = ("selected", flag b)


autocomplete :: Bool -> (Text, Prop m o)
autocomplete b = ("autocomplete", flag b)


$(msum <$> mapM mkTextProp
  [ "id'", "type'", "rel", "href", "placeholder", "value", "src", "title", "accept", "accpetCharset", "action"
  ])


