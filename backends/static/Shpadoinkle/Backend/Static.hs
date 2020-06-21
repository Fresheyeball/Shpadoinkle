{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
  Get your view as plain text, ignoring event listeners.
-}

module Shpadoinkle.Backend.Static ( renderStatic ) where


import           Control.Compactable
import           Data.Monoid         ((<>))
import           Data.Text

import           Shpadoinkle         hiding (name, props, text)


-- | Render @Html@ or @HtmlM@ as @Text@
renderStatic :: IsHtml h p => IsProp p e => h a -> Text
renderStatic = cataH renderTag (const mempty) id


renderTag :: IsProp p e => Text -> [(Text, p a)] -> [Text] -> Text
renderTag tag props cs
  | isSelfClosing tag = renderSelfClosing tag props
  | otherwise         = renderWrapping tag props cs


isSelfClosing :: Text -> Bool
isSelfClosing = flip elem
  [ "area", "base", "br", "embed", "hr", "iframe"
  , "img", "input", "link", "meta", "param", "source", "track" ]


renderWrapping :: IsProp p e => Text -> [(Text, p a)] -> [Text] -> Text
renderWrapping tag props cs = renderOpening tag props <> ">"
  <> mconcat cs <> "</" <> tag <> ">"


renderSelfClosing :: IsProp p e => Text -> [(Text, p a)] -> Text
renderSelfClosing tag props = renderOpening tag props <> " />"


renderOpening :: IsProp p e => Text -> [(Text, p a)] -> Text
renderOpening tag props = let ps = renderProps props in
  "<" <> tag <> (if Data.Text.null ps then mempty else " " <> ps)


renderProps :: IsProp p e => [(Text, p a)] -> Text
renderProps = Data.Text.unwords . fmapMaybe (uncurry renderProp)


renderProp :: IsProp p e => Text -> p a -> Maybe Text
renderProp name = cataProp renderTextProp renderListener renderFlag

  where renderTextProp t = Just $ lice name <> "=\"" <> t <> "\""
        renderListener _ = Nothing
        renderFlag True  = Just name
        renderFlag False = Nothing
        lice = \case
          "className" -> "class"
          x -> x


