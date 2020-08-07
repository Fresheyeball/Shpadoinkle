{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | This module prives a DSL for HTML properties
-- This DSL is entirely optional. You may use the 'Prop' constructors
-- provided by Shpadoinkle core and completely ignore this module.
-- But for those who like a typed DSL with named functions for
-- different properties, and nice overloading, this is for you.
--
-- Unlike Events and Elements, Properties come in one flavor. Vanilla.
--
-- Each named function documents the type of property it constructs
-- whether it be 'Text' or 'Bool'. We also support other types
-- such as `Int`, and `Float`, but via converting them to 'Text' and
-- letting JavaScript weirdness cast them to the correct underlying type.


module Shpadoinkle.Html.Property where


import           Control.Monad       (msum)
import qualified Data.Set            as Set
import           Data.String         hiding (unwords)
import           Data.Text
import           Prelude             hiding (unwords)

import           Shpadoinkle
import           Shpadoinkle.Html.TH


-- | How do we take a non-textual value, and make it text which JavaScript will
-- cast appropriately?
class ToPropText a where toPropText :: a -> Text
instance ToPropText Text where toPropText = id
instance ToPropText Int where toPropText = pack . show
instance ToPropText Float where toPropText = pack . show
instance ToPropText Bool where toPropText = \case True -> "true"; False -> "false"


textProperty :: IsProp p e => ToPropText a => Text -> a -> (Text, p o)
textProperty k = (,) k . textProp . toPropText


newtype ClassList = ClassList { unClassList :: Set.Set Text } deriving (Eq, Ord, Show, Semigroup, Monoid)
class ClassListRep a where asClass :: a -> ClassList
instance ClassListRep Text where asClass = ClassList . Set.fromList . split (== ' ')
instance ClassListRep ClassList where asClass = id
instance ClassListRep (Text, Bool) where asClass (a, b) = if b then asClass a else mempty
instance ClassListRep (ClassList, Bool) where asClass = \case (cl, True) -> cl; _ -> mempty
instance ClassListRep cl => ClassListRep [cl] where asClass = foldMap asClass
instance IsString ClassList where fromString = ClassList . Set.fromList . split (== ' ') . pack


flagProperty :: IsProp p e => Text -> Bool -> (Text, p a)
flagProperty t = (,) t . flagProp


class' :: IsProp p e => ClassListRep cl => cl -> (Text, p a)
class' = className . unwords . Set.toList . unClassList . asClass


className :: IsProp p e => Text -> (Text, p a)
className = textProperty "className"


for' :: IsProp p e => Text -> (Text, p a)
for' = textProperty "htmlFor"


$(msum <$> mapM mkBoolProp
  [ "checked", "selected", "hidden", "autocomplete", "autofocus", "disabled", "autoplay", "controls", "loop"
  , "multiple", "novalidate", "readonly", "required", "ismap", "usemap", "default'", "reversed"
  ])

$(msum <$> mapM mkTextProp
  [ "id'", "type'", "rel", "href", "placeholder", "value", "src", "title"
  , "accept", "action", "acceptCharset", "enctype", "method", "pattern"
  , "max", "min", "step", "wrap", "target", "download", "hreflang", "media", "ping", "shape", "coords"
  , "alt", "preload", "poster", "name'", "kind'", "srclang", "sandbox", "srcdoc", "align"
  , "headers", "scope", "datetime", "pubdate", "manifest", "contextmenu", "draggable"
  , "dropzone", "itemprop", "charset", "content", "property"
  ])

$(msum <$> mapM mkIntProp
 [ "tabIndex", "width", "height" ])


tabbable :: IsProp p e => (Text, p o)
tabbable = tabIndex 0
