{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Html.Utils where


import           Control.Monad
import           Control.Monad.IO.Class      (liftIO)
import           Data.Monoid                 ((<>))
import           Data.Text
import           Language.Javascript.JSaddle

import           Shpadoinkle


addStyle :: Text -> IO ()
addStyle x = void . eval $
  "const l = document.createElement('link') \n" <>
  "l.href = '" <> x <> "' \n" <>
  "l.rel = 'stylesheet' \n" <>
  "document.head.appendChild(l)"


setTitle :: Text -> JSM ()
setTitle t = void . eval $ "document.title = '" <> t <> "';"


getBody :: MonadJSM m => m RawNode
getBody = liftIO $ RawNode <$> eval ("document.body" :: Text)
