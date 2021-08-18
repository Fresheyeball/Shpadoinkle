{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


{-|
  Get your view as plain text, ignoring event listeners.
-}


module Shpadoinkle.Backend.Static ( renderStatic ) where


import           Control.Compactable (Compactable (fmapMaybe))
import           Data.Monoid         (mconcat, (<>))
import qualified Data.Text           as T
import           Data.Text.Lazy      (Text, fromStrict, null, unwords)

import           Shpadoinkle         (Html, Prop (PText), cataH, cataProp)


-- | Render as @Text@
renderStatic :: Html m a -> Text
renderStatic = cataH renderTag (const mempty) fromStrict


renderTag :: T.Text -> [(T.Text, Prop m a)] -> [Text] -> Text
renderTag tag props cs
  | isSelfClosing tag = renderSelfClosing tag props
  | otherwise         = renderWrapping tag props cs


isSelfClosing :: T.Text -> Bool
isSelfClosing = flip elem
  [ "area", "base", "br", "embed", "hr", "iframe"
  , "img", "input", "link", "meta", "param", "source", "track" ]


innerHTML :: T.Text
innerHTML = "innerHTML"


renderWrapping :: T.Text -> [(T.Text, Prop m a)] -> [Text] -> Text
renderWrapping tag props cs =
  renderOpening tag props <> ">"
  <> (case innerHTML `lookup` props of
        Just (PText html) -> fromStrict html
        _                 -> mconcat cs)
  <> "</" <> fromStrict tag <> ">"


renderSelfClosing :: T.Text -> [(T.Text, Prop m a)] -> Text
renderSelfClosing tag props = renderOpening tag props <> " />"


renderOpening :: T.Text -> [(T.Text, Prop m a)] -> Text
renderOpening tag props = let ps = renderProps props in
  "<" <> fromStrict tag <> (if Data.Text.Lazy.null ps then mempty else " " <> ps)


renderProps :: [(T.Text, Prop m a)] -> Text
renderProps = Data.Text.Lazy.unwords . fmapMaybe (uncurry renderProp)


renderProp :: T.Text -> Prop m a -> Maybe Text
renderProp name = cataProp
  (const Nothing)
  renderTextProp
  renderFlag
  (const Nothing)
  (const Nothing)
  where
  renderTextProp t | t == innerHTML = Nothing
                   | otherwise      = Just $ fromStrict (lice name) <> "=\"" <> fromStrict t <> "\""
  renderFlag True  = Just $ fromStrict name
  renderFlag False = Nothing
  lice = \case
    "className" -> "class"
    x           -> x
