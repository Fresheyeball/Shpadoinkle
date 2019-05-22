{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Html.Utils where


import           Control.Monad
import           Data.Monoid                 ((<>))
import           Data.Text
import           Language.Javascript.JSaddle

import           Shpadoinkle


addStyle :: MonadJSM m => Text -> m ()
addStyle x = liftJSM . void . eval $
  "const l = document.createElement('link') \n" <>
  "l.href = '" <> x <> "' \n" <>
  "l.rel = 'stylesheet' \n" <>
  "document.head.appendChild(l)"


setTitle :: MonadJSM m => Text -> m ()
setTitle t = liftJSM . void . eval $ "document.title = '" <> t <> "';"


getBody :: MonadJSM m => m RawNode
getBody = liftJSM $ RawNode <$> eval ("document.body" :: Text)
