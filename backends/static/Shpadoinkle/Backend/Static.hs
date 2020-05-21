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


-- | Render @Shpadoinkle.Html@ as @Text@
renderStatic :: Html m a -> Text
renderStatic = \case
  Node tag props _ | isSelfClosing tag
                     -> renderSelfClosing tag props
  Node tag props cs  -> renderWrapping tag props cs
  Potato _           -> mempty
  TextNode t         -> t


isSelfClosing :: Text -> Bool
isSelfClosing = flip elem
  [ "area", "base", "br", "embed", "hr", "iframe"
  , "img", "input", "link", "meta", "param", "source", "track" ]


renderWrapping :: Text -> [(Text, Prop m a)] -> [Html m a] -> Text
renderWrapping tag props cs = renderOpening tag props <> ">"
  <> mconcat (renderStatic <$> cs) <> "</" <> tag <> ">"


renderSelfClosing :: Text -> [(Text, Prop m a)] -> Text
renderSelfClosing tag props = renderOpening tag props <> " />"


renderOpening :: Text -> [(Text, Prop m a)] -> Text
renderOpening tag props = let ps = renderProps props in
  "<" <> tag <> (if Data.Text.null ps then mempty else " " <> ps)


renderProps :: [(Text, Prop m a)] -> Text
renderProps = Data.Text.unwords . fmapMaybe (uncurry renderProp)


renderProp :: Text -> Prop m a -> Maybe Text
renderProp name = \case
  PListener _ -> Nothing
  PText t     -> Just $ lice name <> "=\"" <> t <> "\""
  PFlag True  -> Just name
  PFlag False -> Nothing
  where
  lice = \case
    "className" -> "class"
    x -> x


