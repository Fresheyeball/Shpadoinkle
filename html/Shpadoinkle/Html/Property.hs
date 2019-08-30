{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Shpadoinkle.Html.Property where


import           Control.Monad       (msum)
import           Data.Maybe          (mapMaybe)
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


textProperty :: ToPropText a => Text -> a -> (Text, Prop m o)
textProperty k = (,) k . PText . toPropText


newtype ClassList = ClassList { unClassList :: Set.Set Text } deriving (Eq, Ord, Show, Semigroup, Monoid)
class ClassListRep a where asClass :: a -> ClassList
instance ClassListRep Text where asClass = ClassList . Set.singleton
instance ClassListRep [Text] where asClass = ClassList . Set.fromList
instance ClassListRep ClassList where asClass = id
instance ClassListRep [(Text, Bool)] where asClass = asClass . mapMaybe (\(a, b) -> if b then Just a else Nothing)
instance ClassListRep (Text, Bool) where asClass = asClass . (:[])
instance IsString ClassList where fromString = ClassList . Set.singleton . pack


flagProperty :: Text -> Bool -> (Text, Prop m o)
flagProperty t = (,) t . flag


className :: ClassListRep cl => cl -> (Text, Prop m o)
className = textProperty "className" . unwords . Set.toList . unClassList . asClass


for' :: Text -> (Text, Prop m o)
for' = textProperty "htmlFor"


$(msum <$> mapM mkBoolProp
  [ "checked", "selected", "hidden", "autocomplete", "autofocus", "disabled", "autoplay", "controls", "loop"
  , "multiple", "novalidate", "readonly", "required", "ismap", "usemap", "default'", "reversed"
  ])

$(msum <$> mapM mkTextProp
  [ "id'", "type'", "rel", "href", "placeholder", "value", "src", "title"
  , "accept", "accpetCharset", "action", "acceptCharset", "enctype", "method", "pattern"
  , "max", "min", "step", "wrap", "target", "download", "hreflang", "media", "ping", "shape", "coords"
  , "alt", "preload", "poster", "name'", "kind'", "srclang", "sandbox", "srcdoc", "align"
  , "headers", "scope", "datetime", "pubdate", "manifest", "contextmenu", "draggable"
  , "dropzone", "itemprop"
  ])


