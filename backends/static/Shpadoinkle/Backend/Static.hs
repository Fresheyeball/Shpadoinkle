{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


{-|
  Get your view as plain text, ignoring event listeners.
-}


module Shpadoinkle.Backend.Static ( renderStatic ) where


import           Control.Compactable (Compactable (fmapMaybe))
import           Data.Monoid         ((<>))
import           Data.Text           (Text, null, unwords)

import           Shpadoinkle         (Html, Prop, cataH, cataProp)


-- | Render as @Text@
renderStatic :: Html m a -> Text
renderStatic = cataH renderTag (const mempty) id


renderTag :: Text -> [(Text, Prop m a)] -> [Text] -> Text
renderTag tag props cs
  | isSelfClosing tag = renderSelfClosing tag props
  | otherwise         = renderWrapping tag props cs


isSelfClosing :: Text -> Bool
isSelfClosing = flip elem
  [ "area", "base", "br", "embed", "hr", "iframe"
  , "img", "input", "link", "meta", "param", "source", "track" ]


renderWrapping :: Text -> [(Text, Prop m a)] -> [Text] -> Text
renderWrapping tag props cs = renderOpening tag props <> ">"
  <> mconcat cs <> "</" <> tag <> ">"


renderSelfClosing :: Text -> [(Text, Prop m a)] -> Text
renderSelfClosing tag props = renderOpening tag props <> " />"


renderOpening :: Text -> [(Text, Prop m a)] -> Text
renderOpening tag props = let ps = renderProps props in
  "<" <> tag <> (if Data.Text.null ps then mempty else " " <> ps)


renderProps :: [(Text, Prop m a)] -> Text
renderProps = Data.Text.unwords . fmapMaybe (uncurry renderProp)


renderProp :: Text -> Prop m a -> Maybe Text
renderProp name = cataProp renderTextProp renderListener renderFlag

  where renderTextProp t = Just $ lice name <> "=\"" <> t <> "\""
        renderListener _ = Nothing
        renderFlag True  = Just name
        renderFlag False = Nothing
        lice = \case
          "className" -> "class"
          x           -> x
