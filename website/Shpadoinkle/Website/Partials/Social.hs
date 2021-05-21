{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Website.Partials.Social where


import           Control.Lens                     ((^.))
import           Data.Generics.Labels             ()
import           Data.Text                        (Text)
import           Shpadoinkle.Html                 (Html, content, h, meta',
                                                   property, text, textProperty)
import           Shpadoinkle.Website.Types.Route  (Route)
import           Shpadoinkle.Website.Types.Social (toSocial)


default (Text)


view :: Route -> [Html m a]
view s' = let s = toSocial s' in
  [ meta' [ property "og:type",        content "website" ]
  , meta' [ property "og:title",       content $ s ^. #title ]
  , meta' [ property "og:description", content $ s ^. #description ]
  , meta' [ textProperty "name" "description", content $ s ^. #description ]
  , h "title" [] [ text $ s ^. #title ]
  ] <> case s ^. #image of
      Just img -> [ meta' [ property "og:image", content img ] ]
      Nothing  -> []
